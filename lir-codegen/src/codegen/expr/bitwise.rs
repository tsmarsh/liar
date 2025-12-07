//! Bitwise operations for LLVM codegen

use inkwell::intrinsics::Intrinsic;
use inkwell::values::BasicValueEnum;
use lir_core::ast::Expr;
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile bitwise operations
    pub(crate) fn compile_bitwise(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::And(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_and(l, r, "and")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in and".to_string())),
                }
            }

            Expr::Or(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_or(l, r, "or")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in or".to_string())),
                }
            }

            Expr::Xor(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_xor(l, r, "xor")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in xor".to_string())),
                }
            }

            Expr::Shl(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_left_shift(l, r, "shl")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in shl".to_string())),
                }
            }

            Expr::LShr(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_right_shift(l, r, false, "lshr")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in lshr".to_string())),
                }
            }

            Expr::AShr(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_right_shift(l, r, true, "ashr")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in ashr".to_string())),
                }
            }

            Expr::Ctpop(val) => {
                let val = self.compile_expr_recursive(val, locals)?.into_int_value();
                let val_type = val.get_type();
                let intrinsic = Intrinsic::find("llvm.ctpop").ok_or_else(|| {
                    CodeGenError::CodeGen("llvm.ctpop intrinsic not found".to_string())
                })?;
                let intrinsic_fn = intrinsic
                    .get_declaration(&self.module, &[val_type.into()])
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("Failed to get ctpop declaration".to_string())
                    })?;
                let call_site = self
                    .builder
                    .build_call(intrinsic_fn, &[val.into()], "ctpop")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodeGenError::CodeGen("ctpop returned no value".to_string()))
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a bitwise expression: {:?}",
                expr
            ))),
        }
    }
}
