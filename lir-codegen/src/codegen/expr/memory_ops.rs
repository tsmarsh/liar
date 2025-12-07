//! Memory operations for LLVM codegen

use inkwell::values::{BasicValue, BasicValueEnum};
use lir_core::ast::Expr;
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile memory operations
    pub(crate) fn compile_memory_op(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Alloca { ty, count } => {
                let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot allocate void type".to_string())
                })?;
                let alloca = if let Some(count_expr) = count {
                    let count_val = self.compile_expr_recursive(count_expr, locals)?;
                    let count_int = count_val.into_int_value();
                    self.builder
                        .build_array_alloca(llvm_ty, count_int, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                } else {
                    self.builder
                        .build_alloca(llvm_ty, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                Ok(alloca.into())
            }

            Expr::Load { ty, ptr } => {
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();
                let llvm_ty = self
                    .param_type_to_basic_type(ty)
                    .ok_or_else(|| CodeGenError::CodeGen("cannot load void type".to_string()))?;
                let loaded = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(loaded)
            }

            Expr::Store { value, ptr } => {
                let val = self.compile_expr_recursive(value, locals)?;
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();
                self.builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                // Store returns void, but we need a BasicValueEnum
                // Return a dummy i32 0 since void can't be a BasicValueEnum
                Ok(self.context.i32_type().const_zero().into())
            }

            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();

                // Get the element type
                let elem_ty = self.gep_type_to_basic_type(ty)?;

                // Compile indices
                let compiled_indices: Vec<inkwell::values::IntValue> = indices
                    .iter()
                    .map(|idx| {
                        self.compile_expr_recursive(idx, locals)
                            .map(|v| v.into_int_value())
                    })
                    .collect::<Result<Vec<_>>>()?;

                // Build GEP
                let gep = if *inbounds {
                    unsafe {
                        self.builder
                            .build_in_bounds_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                } else {
                    unsafe {
                        self.builder
                            .build_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                };

                Ok(gep.into())
            }

            Expr::AtomicLoad { ordering, ty, ptr } => {
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();

                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid atomic load type: {:?}", ty))
                })?;

                let load = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "atomic_load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the load instruction
                let inst = load.as_instruction_value().ok_or_else(|| {
                    CodeGenError::CodeGen("failed to get instruction from load".into())
                })?;
                inst.set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = (ty.bit_width() / 8).max(1);
                inst.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(load)
            }

            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => {
                let val = self.compile_expr_recursive(value, locals)?;
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();

                let store = self
                    .builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the store instruction
                store
                    .set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = Self::value_alignment(&val);
                store.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                // Return null pointer as void indicator
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            Expr::AtomicRMW {
                op,
                ordering,
                ptr,
                value,
            } => {
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();

                let result = self
                    .builder
                    .build_atomicrmw(
                        Self::atomic_rmw_op(op),
                        ptr_val,
                        val,
                        Self::atomic_ordering(ordering),
                    )
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(result.into())
            }

            Expr::CmpXchg {
                ordering,
                ptr,
                expected,
                new_value,
            } => {
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();
                let expected_val = self
                    .compile_expr_recursive(expected, locals)?
                    .into_int_value();
                let new_val = self
                    .compile_expr_recursive(new_value, locals)?
                    .into_int_value();

                let llvm_ordering = Self::atomic_ordering(ordering);
                let result = self
                    .builder
                    .build_cmpxchg(ptr_val, expected_val, new_val, llvm_ordering, llvm_ordering)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(result.into())
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a memory operation: {:?}",
                expr
            ))),
        }
    }
}
