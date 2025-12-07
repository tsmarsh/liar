//! Literal compilation for LLVM codegen

use inkwell::values::BasicValueEnum;
use lir_core::ast::{Expr, FloatValue, ScalarType};
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile literal expressions
    pub(crate) fn compile_literal(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            // Integer literal
            Expr::IntLit {
                ty: scalar_ty,
                value,
            } => {
                let llvm_ty = self.int_type(scalar_ty);
                let val = *value as u64;
                Ok(llvm_ty
                    .const_int(val, scalar_ty != &ScalarType::I1 && *value < 0)
                    .into())
            }

            // Float literal
            Expr::FloatLit {
                ty: scalar_ty,
                value,
            } => {
                let fval = match value {
                    FloatValue::Number(n) => *n,
                    FloatValue::Inf => f64::INFINITY,
                    FloatValue::NegInf => f64::NEG_INFINITY,
                    FloatValue::Nan => f64::NAN,
                };
                match scalar_ty {
                    ScalarType::Float => Ok(self.context.f32_type().const_float(fval).into()),
                    ScalarType::Double => Ok(self.context.f64_type().const_float(fval).into()),
                    _ => Err(CodeGenError::CodeGen("invalid float type".to_string())),
                }
            }

            // String literal - creates a global constant and returns pointer
            Expr::StringLit(s) => {
                // Create a global string constant with null terminator
                let const_array = self.context.const_string(s.as_bytes(), true);
                let array_type = const_array.get_type();

                // Create a private global for this string
                let global = self.module.add_global(array_type, None, ".str");
                global.set_initializer(&const_array);
                global.set_constant(true);

                // Return pointer to the first element
                Ok(global.as_pointer_value().into())
            }

            // Null pointer literal
            Expr::NullPtr => {
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // Global/function reference (returns function pointer)
            Expr::GlobalRef(name) => {
                let func = self.module.get_function(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined function: {}", name))
                })?;
                Ok(func.as_global_value().as_pointer_value().into())
            }

            // Local variable reference
            Expr::LocalRef(name) => locals
                .get(name)
                .copied()
                .ok_or_else(|| CodeGenError::CodeGen(format!("undefined variable: {}", name))),

            _ => Err(CodeGenError::CodeGen(format!(
                "not a literal expression: {:?}",
                expr
            ))),
        }
    }
}
