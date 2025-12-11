//! Arithmetic operations for LLVM codegen

use inkwell::values::BasicValueEnum;
use lir_core::ast::Expr;
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile integer arithmetic operations
    pub(crate) fn compile_int_arithmetic(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "vadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in add".to_string())),
                }
            }

            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "vsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sub".to_string())),
                }
            }

            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "vmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in mul".to_string())),
                }
            }

            Expr::SDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_div(l, r, "sdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sdiv".to_string())),
                }
            }

            Expr::UDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_div(l, r, "udiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in udiv".to_string())),
                }
            }

            Expr::SRem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_rem(l, r, "srem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in srem".to_string())),
                }
            }

            Expr::URem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_rem(l, r, "urem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in urem".to_string())),
                }
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not an integer arithmetic expression: {:?}",
                expr
            ))),
        }
    }

    /// Compile float arithmetic operations
    pub(crate) fn compile_float_arithmetic(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::FAdd(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "fadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "vfadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fadd".to_string())),
                }
            }

            Expr::FSub(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "fsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "vfsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fsub".to_string())),
                }
            }

            Expr::FMul(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "fmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "vfmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fmul".to_string())),
                }
            }

            Expr::FDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "fdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "vfdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fdiv".to_string())),
                }
            }

            Expr::FRem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "frem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "vfrem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in frem".to_string())),
                }
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a float arithmetic expression: {:?}",
                expr
            ))),
        }
    }
}
