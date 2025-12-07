//! Type conversion operations for LLVM codegen

use inkwell::values::BasicValueEnum;
use lir_core::ast::{Expr, ScalarType};
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile type conversion operations
    pub(crate) fn compile_conversion(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Trunc { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_truncate(val, target, "trunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::ZExt { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_z_extend(val, target, "zext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::SExt { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_s_extend(val, target, "sext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::FPTrunc { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = match ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fptrunc target".to_string())),
                };
                Ok(self
                    .builder
                    .build_float_trunc(val, target, "fptrunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::FPExt { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = match ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fpext target".to_string())),
                };
                Ok(self
                    .builder
                    .build_float_ext(val, target, "fpext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::FPToUI { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_float_to_unsigned_int(val, target, "fptoui")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::FPToSI { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_float_to_signed_int(val, target, "fptosi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::UIToFP { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = match ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid uitofp target".to_string())),
                };
                Ok(self
                    .builder
                    .build_unsigned_int_to_float(val, target, "uitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::SIToFP { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = match ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid sitofp target".to_string())),
                };
                Ok(self
                    .builder
                    .build_signed_int_to_float(val, target, "sitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a conversion expression: {:?}",
                expr
            ))),
        }
    }
}
