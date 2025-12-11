//! Memory operations for LLVM codegen
//!
//! Reference counting, bounds checking, and memory allocation.

use inkwell::values::{BasicValueEnum, FunctionValue};
use lir_core::ast::{Expr, ScalarType};
use std::collections::HashMap;

use super::{CodeGenError, Result};

/// Memory-related methods for CodeGen
impl<'ctx> super::CodeGen<'ctx> {
    /// Get required alignment for a value (in bytes)
    pub(crate) fn value_alignment(val: &BasicValueEnum<'ctx>) -> u32 {
        match val {
            BasicValueEnum::IntValue(v) => (v.get_type().get_bit_width() / 8).max(1),
            BasicValueEnum::FloatValue(_) => {
                // f32 = 4 bytes, f64 = 8 bytes
                // inkwell doesn't expose bit width directly for floats, but we can use the type
                // For now, assume 8 bytes for double, 4 for float
                8 // Default to 8 for safety
            }
            BasicValueEnum::PointerValue(_) => 8, // Assume 64-bit pointers
            BasicValueEnum::VectorValue(_) => 16, // Vectors typically need 16-byte alignment
            BasicValueEnum::ScalableVectorValue(_) => 16, // Scalable vectors
            BasicValueEnum::ArrayValue(_) => 8,
            BasicValueEnum::StructValue(_) => 8,
        }
    }

    /// Check if an index expression is a constant within bounds
    pub(crate) fn is_constant_in_bounds(index: &Expr, size: u32) -> bool {
        // Check if the index is a constant integer literal within bounds
        if let Expr::IntLit { value, .. } = index {
            if *value >= 0 && (*value as u32) < size {
                return true;
            }
        }
        false
    }

    /// Emit runtime bounds check for array access
    pub(crate) fn emit_bounds_check(
        &self,
        idx: inkwell::values::IntValue<'ctx>,
        size: u32,
    ) -> Result<()> {
        let current_fn = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodeGenError::CodeGen("no insert block".into()))?
            .get_parent()
            .ok_or_else(|| CodeGenError::CodeGen("block has no parent".into()))?;

        let i64_type = self.context.i64_type();
        let size_val = i64_type.const_int(size as u64, false);

        // Extend idx to i64 if needed for comparison
        let idx_i64 = if idx.get_type().get_bit_width() < 64 {
            self.builder
                .build_int_z_extend(idx, i64_type, "idx_ext")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        } else {
            idx
        };

        // Check: idx < size (unsigned comparison)
        let in_bounds = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                idx_i64,
                size_val,
                "bounds_check",
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Create blocks for bounds check
        let ok_block = self.context.append_basic_block(current_fn, "bounds_ok");
        let panic_block = self.context.append_basic_block(current_fn, "bounds_panic");

        self.builder
            .build_conditional_branch(in_bounds, ok_block, panic_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Panic block: call abort
        self.builder.position_at_end(panic_block);
        // Use libc abort() - simpler than trying to use LLVM intrinsics
        let abort_fn = self.module.get_function("abort").unwrap_or_else(|| {
            let void_type = self.context.void_type();
            let fn_type = void_type.fn_type(&[], false);
            self.module
                .add_function("abort", fn_type, Some(inkwell::module::Linkage::External))
        });
        self.builder
            .build_call(abort_fn, &[], "")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        self.builder
            .build_unreachable()
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Continue in OK block
        self.builder.position_at_end(ok_block);

        Ok(())
    }

    /// Compile rc-alloc: allocate with refcount header
    pub(crate) fn compile_rc_alloc(&self, elem_type: &ScalarType) -> Result<BasicValueEnum<'ctx>> {
        // Get or declare malloc
        let malloc_fn = self.get_or_declare_malloc()?;

        // Calculate size: sizeof(i64) + sizeof(T)
        let elem_size = elem_type.bit_width() / 8;
        let total_size = 8 + elem_size as u64; // refcount (i64) + data

        let i64_type = self.context.i64_type();
        let size_val = i64_type.const_int(total_size, false);

        // Call malloc
        let call_site = self
            .builder
            .build_call(malloc_fn, &[size_val.into()], "rc_base")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let base_ptr = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("malloc returned void".to_string()))?
            .into_pointer_value();

        // Initialize refcount to 1
        let one = i64_type.const_int(1, false);
        self.builder
            .build_store(base_ptr, one)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return pointer to data (8 bytes after base)
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let i8_type = self.context.i8_type();
        let eight = i64_type.const_int(8, false);
        let data_ptr = unsafe {
            self.builder
                .build_gep(i8_type, base_ptr, &[eight], "rc_data")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        let data_ptr = self
            .builder
            .build_pointer_cast(data_ptr, ptr_type, "rc_ptr")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(data_ptr.into())
    }

    /// Compile rc-clone: increment refcount atomically
    pub(crate) fn compile_rc_clone(
        &self,
        value: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self
            .compile_expr_recursive(value, locals)?
            .into_pointer_value();
        self.compile_rc_clone_ptr(data_ptr)
    }

    /// Helper for rc-clone with pre-computed pointer
    pub(crate) fn compile_rc_clone_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let one = i64_type.const_int(1, false);
        self.builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Add,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(data_ptr.into())
    }

    /// Compile rc-drop: decrement refcount, free if zero
    pub(crate) fn compile_rc_drop(
        &self,
        value: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self
            .compile_expr_recursive(value, locals)?
            .into_pointer_value();
        self.compile_rc_drop_ptr(data_ptr)
    }

    /// Helper for rc-drop with pre-computed pointer
    pub(crate) fn compile_rc_drop_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let one = i64_type.const_int(1, false);
        let old_rc = self
            .builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Sub,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let was_one = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, old_rc, one, "was_one")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let current_fn = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodeGenError::CodeGen("no insert block".to_string()))?
            .get_parent()
            .ok_or_else(|| CodeGenError::CodeGen("no parent function".to_string()))?;

        let free_block = self.context.append_basic_block(current_fn, "rc_free");
        let cont_block = self.context.append_basic_block(current_fn, "rc_cont");

        self.builder
            .build_conditional_branch(was_one, free_block, cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        self.builder.position_at_end(free_block);
        let free_fn = self.get_or_declare_free()?;
        self.builder
            .build_call(free_fn, &[rc_ptr.into()], "")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        self.builder.position_at_end(cont_block);

        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        Ok(ptr_type.const_null().into())
    }

    /// Compile rc-count: read refcount
    pub(crate) fn compile_rc_count(
        &self,
        value: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self
            .compile_expr_recursive(value, locals)?
            .into_pointer_value();
        self.compile_rc_count_ptr(data_ptr)
    }

    /// Helper for rc-count with pre-computed pointer
    pub(crate) fn compile_rc_count_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let count = self
            .builder
            .build_load(i64_type, rc_ptr, "rc_count")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(count)
    }

    /// Get or declare malloc function
    pub(crate) fn get_or_declare_malloc(&self) -> Result<FunctionValue<'ctx>> {
        if let Some(fn_val) = self.module.get_function("malloc") {
            return Ok(fn_val);
        }

        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
        Ok(self.module.add_function("malloc", fn_type, None))
    }

    /// Get or declare free function
    pub(crate) fn get_or_declare_free(&self) -> Result<FunctionValue<'ctx>> {
        if let Some(fn_val) = self.module.get_function("free") {
            return Ok(fn_val);
        }

        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        Ok(self.module.add_function("free", fn_type, None))
    }
}
