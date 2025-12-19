//! Expression compilation for LLVM codegen
//!
//! Handles compilation of all lIR expression types to LLVM IR.

mod arithmetic;
mod bitwise;
mod comparison;
mod conversion;
mod literals;
mod memory_ops;
mod vector;

use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};
use lir_core::ast::{Expr, ScalarType, Type};
use std::collections::HashMap;

use super::{CodeGenError, Result};

/// Expression compilation methods for CodeGen
impl<'ctx> super::CodeGen<'ctx> {
    /// Compile expression with local variable context (for function bodies)
    pub fn compile_expr_with_locals(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            // Local variable reference
            Expr::LocalRef(name) => {
                let value = locals.get(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined variable: {}", name))
                })?;
                Ok(Some(*value))
            }

            // Return instruction
            Expr::Ret(value) => {
                match value {
                    Some(v) => {
                        let val = self.compile_expr_with_locals(v, locals)?.ok_or_else(|| {
                            CodeGenError::CodeGen("ret value has no result".to_string())
                        })?;
                        self.builder
                            .build_return(Some(&val))
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                    None => {
                        self.builder
                            .build_return(None)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                }
                Ok(None) // ret doesn't produce a value
            }

            // For other expressions, delegate to the original compile_expr
            // but we need to handle sub-expressions with locals
            _ => {
                let val = self.compile_expr_recursive(expr, locals)?;
                Ok(Some(val))
            }
        }
    }

    /// Compile expression recursively with locals context
    pub(crate) fn compile_expr_recursive(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            // Local variable reference
            Expr::LocalRef(name) => locals
                .get(name)
                .copied()
                .ok_or_else(|| CodeGenError::CodeGen(format!("undefined variable: {}", name))),

            // Delegate literals and operations
            _ => self.compile_expr_inner(expr, locals),
        }
    }

    /// Inner expression compilation with locals support
    /// Dispatches to category-specific modules
    fn compile_expr_inner(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            // Literals
            Expr::IntLit { .. }
            | Expr::FloatLit { .. }
            | Expr::StringLit(_)
            | Expr::NullPtr
            | Expr::GlobalRef(_)
            | Expr::LocalRef(_) => self.compile_literal(expr, locals),

            // Integer arithmetic
            Expr::Add(_, _)
            | Expr::Sub(_, _)
            | Expr::Mul(_, _)
            | Expr::SDiv(_, _)
            | Expr::UDiv(_, _)
            | Expr::SRem(_, _)
            | Expr::URem(_, _) => self.compile_int_arithmetic(expr, locals),

            // Float arithmetic
            Expr::FAdd(_, _)
            | Expr::FSub(_, _)
            | Expr::FMul(_, _)
            | Expr::FDiv(_, _)
            | Expr::FRem(_, _) => self.compile_float_arithmetic(expr, locals),

            // Bitwise operations
            Expr::And(_, _)
            | Expr::Or(_, _)
            | Expr::Xor(_, _)
            | Expr::Shl(_, _)
            | Expr::LShr(_, _)
            | Expr::AShr(_, _)
            | Expr::Ctpop(_) => self.compile_bitwise(expr, locals),

            // Comparisons
            Expr::ICmp { .. } | Expr::FCmp { .. } => self.compile_comparison(expr, locals),

            // Type conversions
            Expr::Trunc { .. }
            | Expr::ZExt { .. }
            | Expr::SExt { .. }
            | Expr::FPTrunc { .. }
            | Expr::FPExt { .. }
            | Expr::FPToUI { .. }
            | Expr::FPToSI { .. }
            | Expr::UIToFP { .. }
            | Expr::SIToFP { .. }
            | Expr::IntToPtr { .. }
            | Expr::PtrToInt { .. } => self.compile_conversion(expr, locals),

            // Memory operations
            Expr::Alloca { .. }
            | Expr::Load { .. }
            | Expr::Store { .. }
            | Expr::GetElementPtr { .. }
            | Expr::AtomicLoad { .. }
            | Expr::AtomicStore { .. }
            | Expr::AtomicRMW { .. }
            | Expr::CmpXchg { .. } => self.compile_memory_op(expr, locals),

            // Vector operations
            Expr::VectorLit { .. }
            | Expr::ExtractElement { .. }
            | Expr::InsertElement { .. }
            | Expr::ShuffleVector { .. } => self.compile_vector_op(expr, locals),

            // Select (control flow)
            Expr::Select { .. } => self.compile_select(expr, locals),

            // Let bindings
            Expr::Let { bindings, body } => self.compile_let_expr(bindings, body, locals),

            // Function calls
            Expr::Call { name, args } => self.compile_call(name, args, locals),
            Expr::IndirectCall {
                fn_ptr,
                ret_ty,
                args,
            } => self.compile_indirect_call(fn_ptr, ret_ty, args, locals),
            Expr::TailCall { .. } | Expr::IndirectTailCall { .. } => Err(CodeGenError::CodeGen(
                "tailcall requires block context".to_string(),
            )),

            // Heap-allocated struct with ownership
            Expr::HeapStruct {
                struct_name,
                fields,
            } => self.compile_heap_struct(struct_name, fields, locals),

            // Reference counting
            Expr::RcAlloc { elem_type } => self.compile_rc_alloc(elem_type),
            Expr::RcClone { value } => self.compile_rc_clone(value, locals),
            Expr::RcDrop { value } => self.compile_rc_drop(value, locals),
            Expr::RcCount { value } => self.compile_rc_count(value, locals),
            Expr::RcPtr { value } => {
                // Get pointer to RC data
                let rc_ptr = self
                    .compile_expr_recursive(value, locals)?
                    .into_pointer_value();
                Ok(rc_ptr.into())
            }

            // Memory deallocation
            Expr::Free { ptr } => {
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();
                let free_fn = self.get_or_declare_free()?;
                self.builder
                    .build_call(free_fn, &[ptr_val.into()], "")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                // Free returns void, return null pointer as unit value
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // Ownership operations
            Expr::AllocOwn { elem_type } => {
                // Allocate memory for an owned value
                let llvm_ty = self.int_type(elem_type);
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, "own")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(alloca.into())
            }

            Expr::Drop { value } => {
                // For now, drop is a no-op at LLVM level
                // In a full implementation, this would call destructors
                let _ = self.compile_expr_recursive(value, locals)?;
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            Expr::Move { value } => {
                // Move just returns the value - ownership transfer is semantic
                self.compile_expr_recursive(value, locals)
            }

            Expr::BorrowRef { value } => {
                // Borrow returns a pointer to the value
                // For now, just return the value (assumes it's already a pointer)
                self.compile_expr_recursive(value, locals)
            }
            Expr::BorrowRefMut { value } => {
                // Mutable borrow returns a pointer to the value
                self.compile_expr_recursive(value, locals)
            }

            // Array operations
            Expr::ArrayAlloc { elem_type, size } => self.compile_array_alloc(elem_type, *size),
            Expr::ArrayGet {
                elem_type,
                size,
                array,
                index,
            } => self.compile_array_get(elem_type, *size, array, index, locals),
            Expr::ArraySet {
                elem_type,
                size,
                array,
                index,
                value,
            } => self.compile_array_set(elem_type, *size, array, index, value, locals),
            Expr::ArrayLen { size } => {
                // Return the compile-time known size
                Ok(self
                    .context
                    .i64_type()
                    .const_int(*size as u64, false)
                    .into())
            }
            Expr::HeapArray { elem_type, size } => self.compile_heap_array(elem_type, *size),
            Expr::HeapArrayDyn { elem_type, size } => {
                self.compile_heap_array_dyn(elem_type, size, locals)
            }
            Expr::ArrayCopy {
                elem_type,
                size,
                dest,
                src,
            } => self.compile_array_copy(elem_type, *size, dest, src, locals),

            // Pointer array operations
            Expr::PtrArrayAlloc { size } => self.compile_ptr_array_alloc(*size),
            Expr::PtrArrayGet { size, array, index } => {
                self.compile_ptr_array_get(*size, array, index, locals)
            }
            Expr::PtrArraySet {
                size,
                array,
                index,
                value,
            } => self.compile_ptr_array_set(*size, array, index, value, locals),

            // Struct operations
            Expr::StructLit(fields) => self.compile_struct_lit(fields, locals),
            Expr::ExtractValue { aggregate, indices } => {
                // Use first index for now
                let index = indices.first().copied().unwrap_or(0);
                self.compile_extract_value(aggregate, index, locals)
            }
            Expr::InsertValue {
                aggregate,
                value,
                indices,
            } => {
                let index = indices.first().copied().unwrap_or(0);
                self.compile_insert_value(aggregate, value, index, locals)
            }

            // Instructions that don't produce values
            Expr::Ret(_) => Err(CodeGenError::CodeGen(
                "ret should be handled at statement level".to_string(),
            )),
            Expr::Br(_) => Err(CodeGenError::CodeGen(
                "br should be handled at statement level".to_string(),
            )),
            Expr::Phi { .. } => Err(CodeGenError::CodeGen(
                "phi should be handled at statement level".to_string(),
            )),

            // ArrayPtr - get pointer to array data
            Expr::ArrayPtr { array } => {
                // Just return the array pointer directly
                self.compile_expr_recursive(array, locals)
            }

            // Fence - memory barrier
            Expr::Fence { ordering } => {
                self.builder
                    .build_fence(Self::atomic_ordering(ordering), 1, "")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                // Return a dummy value since fence doesn't produce a result
                Ok(self.context.i64_type().const_int(0, false).into())
            }
        }
    }

    /// Compile a standalone expression (without function context)
    pub fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let empty_locals = HashMap::new();
        self.compile_expr_recursive(expr, &empty_locals)
    }

    /// Helper for let bindings - compile with a locals map
    fn compile_let_expr(
        &self,
        bindings: &[(String, Box<Expr>)],
        body: &[Expr],
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let mut new_locals = locals.clone();

        // Evaluate each binding and add to locals
        for (name, expr) in bindings {
            let value = self.compile_with_locals(expr, &new_locals)?;
            new_locals.insert(name.clone(), value);
        }

        // Evaluate body expressions, return last
        // Note: Tail calls (TailCall/IndirectTailCall) in the body will error
        // because they require block context for the return instruction.
        // This is expected - tail calls should only appear in function-level Let.
        let mut result = None;
        for expr in body {
            result = Some(self.compile_with_locals(expr, &new_locals)?);
        }

        result.ok_or_else(|| CodeGenError::CodeGen("empty let body".to_string()))
    }

    /// Compile expression with locals context (for let bindings)
    /// This is the compile_with_locals from the original file
    pub(crate) fn compile_with_locals(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        self.compile_expr_recursive(expr, locals)
    }

    /// Compile a function call
    fn compile_call(
        &self,
        name: &str,
        args: &[Expr],
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let function = self
            .module
            .get_function(name)
            .ok_or_else(|| CodeGenError::CodeGen(format!("undefined function: {}", name)))?;

        let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
        for arg in args {
            let val = self.compile_expr_recursive(arg, locals)?;
            compiled_args.push(val.into());
        }

        let call = self
            .builder
            .build_call(function, &compiled_args, "call")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Try to get return value (may be void)
        call.try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("call returned void".to_string()))
    }

    /// Compile an indirect function call
    fn compile_indirect_call(
        &self,
        fn_ptr: &Expr,
        ret_ty: &lir_core::ast::ParamType,
        args: &[Expr],
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        // Compile the function pointer
        let fn_ptr_val = self.compile_expr_recursive(fn_ptr, locals)?;

        // Build the function type from ret_ty and arg count
        let ret_llvm_ty = self.param_type_to_basic_type(ret_ty);
        let arg_types: Vec<inkwell::types::BasicMetadataTypeEnum> = args
            .iter()
            .map(|_| {
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into()
            })
            .collect();
        let fn_type = match ret_llvm_ty {
            Some(basic_ty) => basic_ty.fn_type(&arg_types, false),
            None => self.context.void_type().fn_type(&arg_types, false),
        };

        // Compile the arguments
        let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
        for arg in args {
            let val = self.compile_expr_recursive(arg, locals)?;
            compiled_args.push(val.into());
        }

        // Build indirect call
        let call = self
            .builder
            .build_indirect_call(
                fn_type,
                fn_ptr_val.into_pointer_value(),
                &compiled_args,
                "indirect_call",
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Get return value
        call.try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("indirect call returned void".to_string()))
    }

    /// Compile a select (ternary) expression
    fn compile_select(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        if let Expr::Select {
            cond,
            true_val,
            false_val,
        } = expr
        {
            let cond_val = self.compile_expr_recursive(cond, locals)?.into_int_value();
            let true_v = self.compile_expr_recursive(true_val, locals)?;
            let false_v = self.compile_expr_recursive(false_val, locals)?;
            Ok(self
                .builder
                .build_select(cond_val, true_v, false_v, "select")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
        } else {
            unreachable!()
        }
    }

    /// Compile array allocation
    fn compile_array_alloc(
        &self,
        elem_type: &ScalarType,
        size: u32,
    ) -> Result<BasicValueEnum<'ctx>> {
        let llvm_ty = self.int_type(elem_type);
        let count = self.context.i32_type().const_int(size as u64, false);
        let alloca = self
            .builder
            .build_array_alloca(llvm_ty, count, "array")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        Ok(alloca.into())
    }

    /// Compile array get with bounds checking
    fn compile_array_get(
        &self,
        elem_type: &ScalarType,
        size: u32,
        array: &Expr,
        index: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let arr_ptr = self
            .compile_expr_recursive(array, locals)?
            .into_pointer_value();
        let idx = self.compile_expr_recursive(index, locals)?.into_int_value();

        // Emit bounds check if index is not a constant in bounds
        if !Self::is_constant_in_bounds(index, size) && size > 0 {
            self.emit_bounds_check(idx, size)?;
        }

        let llvm_ty = self.int_type(elem_type);
        let elem_ptr = unsafe {
            self.builder
                .build_gep(llvm_ty, arr_ptr, &[idx], "arrayidx")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        let loaded = self
            .builder
            .build_load(llvm_ty, elem_ptr, "arrayget")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        Ok(loaded)
    }

    /// Compile array set with bounds checking
    fn compile_array_set(
        &self,
        elem_type: &ScalarType,
        size: u32,
        array: &Expr,
        index: &Expr,
        value: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let arr_ptr = self
            .compile_expr_recursive(array, locals)?
            .into_pointer_value();
        let idx = self.compile_expr_recursive(index, locals)?.into_int_value();
        let val = self.compile_expr_recursive(value, locals)?;

        // Emit bounds check if index is not a constant in bounds
        if !Self::is_constant_in_bounds(index, size) && size > 0 {
            self.emit_bounds_check(idx, size)?;
        }

        let llvm_ty = self.int_type(elem_type);
        let elem_ptr = unsafe {
            self.builder
                .build_gep(llvm_ty, arr_ptr, &[idx], "arrayidx")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        self.builder
            .build_store(elem_ptr, val)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return the value that was set (as i64)
        Ok(self.context.i64_type().const_zero().into())
    }

    /// Compile heap array allocation via malloc
    fn compile_heap_array(
        &self,
        elem_type: &ScalarType,
        size: u32,
    ) -> Result<BasicValueEnum<'ctx>> {
        let llvm_ty = self.int_type(elem_type);
        let elem_size = llvm_ty.size_of();
        let total_size = self.context.i64_type().const_int(size as u64, false);
        let byte_size = self
            .builder
            .build_int_mul(elem_size, total_size, "bytesize")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Call malloc
        let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
            let fn_type = self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(&[self.context.i64_type().into()], false);
            self.module.add_function("malloc", fn_type, None)
        });

        let call_site = self
            .builder
            .build_call(malloc_fn, &[byte_size.into()], "heaparray")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let ptr = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("malloc returned void".to_string()))?;

        Ok(ptr)
    }

    /// Compile dynamic heap array allocation via malloc with runtime size
    fn compile_heap_array_dyn(
        &self,
        elem_type: &ScalarType,
        size_expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let llvm_ty = self.int_type(elem_type);
        let elem_size = llvm_ty.size_of();

        // Compile the size expression
        let size_val = self
            .compile_expr_recursive(size_expr, locals)?
            .into_int_value();

        let byte_size = self
            .builder
            .build_int_mul(elem_size, size_val, "bytesize")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Call malloc
        let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
            let fn_type = self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(&[self.context.i64_type().into()], false);
            self.module.add_function("malloc", fn_type, None)
        });

        let call_site = self
            .builder
            .build_call(malloc_fn, &[byte_size.into()], "heaparraydyn")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let ptr = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("malloc returned void".to_string()))?;

        Ok(ptr)
    }

    /// Compile array copy via memcpy
    fn compile_array_copy(
        &self,
        elem_type: &ScalarType,
        size: u32,
        dest: &Expr,
        src: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let dest_ptr = self
            .compile_expr_recursive(dest, locals)?
            .into_pointer_value();
        let src_ptr = self
            .compile_expr_recursive(src, locals)?
            .into_pointer_value();

        let llvm_ty = self.int_type(elem_type);
        let elem_size = llvm_ty.size_of();
        let count = self.context.i64_type().const_int(size as u64, false);
        let byte_size = self
            .builder
            .build_int_mul(elem_size, count, "copysize")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Use LLVM's memcpy intrinsic
        self.builder
            .build_memcpy(dest_ptr, 8, src_ptr, 8, byte_size)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return dest pointer
        Ok(dest_ptr.into())
    }

    /// Compile pointer array allocation via malloc
    fn compile_ptr_array_alloc(&self, size: u32) -> Result<BasicValueEnum<'ctx>> {
        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let ptr_size = ptr_ty.size_of();
        let count = self.context.i64_type().const_int(size as u64, false);
        let byte_size = self
            .builder
            .build_int_mul(ptr_size, count, "bytesize")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Call malloc
        let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
            let fn_type = ptr_ty.fn_type(&[self.context.i64_type().into()], false);
            self.module.add_function("malloc", fn_type, None)
        });

        let call_site = self
            .builder
            .build_call(malloc_fn, &[byte_size.into()], "ptrarrayalloc")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let ptr = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("malloc returned void".to_string()))?;

        Ok(ptr)
    }

    /// Compile pointer array get with bounds checking
    fn compile_ptr_array_get(
        &self,
        size: u32,
        array: &Expr,
        index: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let arr_ptr = self
            .compile_expr_recursive(array, locals)?
            .into_pointer_value();
        let idx = self.compile_expr_recursive(index, locals)?.into_int_value();

        // Emit bounds check if index is not a constant in bounds
        if !Self::is_constant_in_bounds(index, size) && size > 0 {
            self.emit_bounds_check(idx, size)?;
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let elem_ptr = unsafe {
            self.builder
                .build_gep(ptr_ty, arr_ptr, &[idx], "ptrarrayidx")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        let loaded = self
            .builder
            .build_load(ptr_ty, elem_ptr, "ptrarrayget")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        Ok(loaded)
    }

    /// Compile pointer array set with bounds checking
    fn compile_ptr_array_set(
        &self,
        size: u32,
        array: &Expr,
        index: &Expr,
        value: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let arr_ptr = self
            .compile_expr_recursive(array, locals)?
            .into_pointer_value();
        let idx = self.compile_expr_recursive(index, locals)?.into_int_value();
        let val = self.compile_expr_recursive(value, locals)?;

        // Emit bounds check if index is not a constant in bounds
        if !Self::is_constant_in_bounds(index, size) && size > 0 {
            self.emit_bounds_check(idx, size)?;
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let elem_ptr = unsafe {
            self.builder
                .build_gep(ptr_ty, arr_ptr, &[idx], "ptrarrayidx")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        self.builder
            .build_store(elem_ptr, val)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return void (as i64 zero)
        Ok(self.context.i64_type().const_zero().into())
    }

    /// Compile struct literal
    fn compile_struct_lit(
        &self,
        fields: &[Expr],
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let field_vals: Vec<BasicValueEnum<'ctx>> = fields
            .iter()
            .map(|f| self.compile_expr_recursive(f, locals))
            .collect::<Result<Vec<_>>>()?;

        // Get field types
        let field_types: Vec<_> = field_vals.iter().map(|v| v.get_type()).collect();
        let struct_type = self.context.struct_type(&field_types, false);

        // Build struct value
        let mut struct_val = struct_type.get_undef();
        for (i, val) in field_vals.iter().enumerate() {
            struct_val = self
                .builder
                .build_insert_value(struct_val, *val, i as u32, "struct_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                .into_struct_value();
        }

        Ok(struct_val.into())
    }

    /// Compile extractvalue
    fn compile_extract_value(
        &self,
        aggregate: &Expr,
        index: u32,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let agg_val = self.compile_expr_recursive(aggregate, locals)?;
        let extracted = self
            .builder
            .build_extract_value(agg_val.into_struct_value(), index, "extract")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        Ok(extracted)
    }

    /// Compile insertvalue
    fn compile_insert_value(
        &self,
        aggregate: &Expr,
        value: &Expr,
        index: u32,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let agg_val = self.compile_expr_recursive(aggregate, locals)?;
        let val = self.compile_expr_recursive(value, locals)?;
        let inserted = self
            .builder
            .build_insert_value(agg_val.into_struct_value(), val, index, "insert")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        Ok(inserted.into_struct_value().into())
    }

    /// Create a JIT function that evaluates the expression and returns the result
    /// The type should be pre-computed by the caller via type checking.
    pub fn create_eval_function(&self, expr: &Expr, ty: &Type) -> Result<()> {
        let fn_type = match ty {
            Type::Scalar(ScalarType::Float) => self.context.f32_type().fn_type(&[], false),
            Type::Scalar(ScalarType::Double) => self.context.f64_type().fn_type(&[], false),
            Type::Scalar(s) => {
                // All other scalars are integers
                self.int_type(s).fn_type(&[], false)
            }
            Type::Vector(_) => {
                // For vectors, we pass an output pointer as a parameter
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                self.context.void_type().fn_type(&[ptr_type.into()], false)
            }
            Type::Ptr => {
                // Pointer return type
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                ptr_type.fn_type(&[], false)
            }
        };

        let function = self.module.add_function("__lir_eval", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let result = self.compile_expr(expr)?;

        match &ty {
            Type::Vector(vt) => {
                // Store result to output pointer
                let out_ptr = function.get_first_param().unwrap().into_pointer_value();

                // For i1 vectors, we need special handling: zext each element to i8
                // because LLVM packs i1 vectors as bits, but we want them as bytes
                if vt.element == ScalarType::I1 {
                    let vec = result.into_vector_value();
                    let i8_type = self.context.i8_type();
                    for i in 0..vt.count {
                        let elem = self
                            .builder
                            .build_extract_element(
                                vec,
                                self.context.i32_type().const_int(i as u64, false),
                                "elem",
                            )
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                        let extended = self
                            .builder
                            .build_int_z_extend(elem.into_int_value(), i8_type, "zext")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                        let elem_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    i8_type,
                                    out_ptr,
                                    &[self.context.i32_type().const_int(i as u64, false)],
                                    "elemptr",
                                )
                                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        };
                        self.builder
                            .build_store(elem_ptr, extended)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                } else {
                    self.builder
                        .build_store(out_ptr, result)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }
                self.builder
                    .build_return(None)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
            }
            _ => {
                self.builder
                    .build_return(Some(&result))
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
            }
        }

        Ok(())
    }
}
