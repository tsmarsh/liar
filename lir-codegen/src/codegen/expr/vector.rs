//! Vector operations for LLVM codegen

use inkwell::values::{BasicValueEnum, VectorValue as LLVMVectorValue};
use lir_core::ast::Expr;
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile vector operations
    pub(crate) fn compile_vector_op(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::VectorLit { ty, elements } => {
                let vec_type = self.vec_type(ty);
                let mut vals: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(elements.len());
                for elem in elements {
                    vals.push(self.compile_expr_recursive(elem, locals)?);
                }

                // Build vector from elements
                let mut vec: LLVMVectorValue<'ctx> = vec_type.get_undef();
                for (i, val) in vals.iter().enumerate() {
                    let idx = self.context.i32_type().const_int(i as u64, false);
                    vec = self
                        .builder
                        .build_insert_element(vec, *val, idx, "vec_insert")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }
                Ok(vec.into())
            }

            Expr::ExtractElement { vec, idx } => {
                let vec_val = self
                    .compile_expr_recursive(vec, locals)?
                    .into_vector_value();
                let idx_val = self.compile_expr_recursive(idx, locals)?.into_int_value();
                Ok(self
                    .builder
                    .build_extract_element(vec_val, idx_val, "extract")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            Expr::InsertElement { vec, val, idx } => {
                let vec_val = self
                    .compile_expr_recursive(vec, locals)?
                    .into_vector_value();
                let elem_val = self.compile_expr_recursive(val, locals)?;
                let idx_val = self.compile_expr_recursive(idx, locals)?.into_int_value();
                Ok(self
                    .builder
                    .build_insert_element(vec_val, elem_val, idx_val, "insert")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::ShuffleVector { vec1, vec2, mask } => {
                let vec1_val = self
                    .compile_expr_recursive(vec1, locals)?
                    .into_vector_value();
                let vec2_val = self
                    .compile_expr_recursive(vec2, locals)?
                    .into_vector_value();
                let mask_val = self
                    .compile_expr_recursive(mask, locals)?
                    .into_vector_value();
                Ok(self
                    .builder
                    .build_shuffle_vector(vec1_val, vec2_val, mask_val, "shuffle")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a vector operation: {:?}",
                expr
            ))),
        }
    }
}
