//! Comparison operations for LLVM codegen

use inkwell::values::BasicValueEnum;
use inkwell::{FloatPredicate, IntPredicate};
use lir_core::ast::{Expr, FCmpPred, ICmpPred};
use std::collections::HashMap;

use crate::codegen::{CodeGenError, Result};

impl<'ctx> crate::codegen::CodeGen<'ctx> {
    /// Compile comparison operations
    pub(crate) fn compile_comparison(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::ICmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                let llvm_pred = match pred {
                    ICmpPred::Eq => IntPredicate::EQ,
                    ICmpPred::Ne => IntPredicate::NE,
                    ICmpPred::Slt => IntPredicate::SLT,
                    ICmpPred::Sle => IntPredicate::SLE,
                    ICmpPred::Sgt => IntPredicate::SGT,
                    ICmpPred::Sge => IntPredicate::SGE,
                    ICmpPred::Ult => IntPredicate::ULT,
                    ICmpPred::Ule => IntPredicate::ULE,
                    ICmpPred::Ugt => IntPredicate::UGT,
                    ICmpPred::Uge => IntPredicate::UGE,
                };
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_compare(llvm_pred, l, r, "icmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in icmp".to_string())),
                }
            }

            Expr::FCmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                let llvm_pred = match pred {
                    FCmpPred::Oeq => FloatPredicate::OEQ,
                    FCmpPred::One => FloatPredicate::ONE,
                    FCmpPred::Olt => FloatPredicate::OLT,
                    FCmpPred::Ole => FloatPredicate::OLE,
                    FCmpPred::Ogt => FloatPredicate::OGT,
                    FCmpPred::Oge => FloatPredicate::OGE,
                    FCmpPred::Ord => FloatPredicate::ORD,
                    FCmpPred::Ueq => FloatPredicate::UEQ,
                    FCmpPred::Une => FloatPredicate::UNE,
                    FCmpPred::Ult => FloatPredicate::ULT,
                    FCmpPred::Ule => FloatPredicate::ULE,
                    FCmpPred::Ugt => FloatPredicate::UGT,
                    FCmpPred::Uge => FloatPredicate::UGE,
                    FCmpPred::Uno => FloatPredicate::UNO,
                };
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_compare(llvm_pred, l, r, "fcmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fcmp".to_string())),
                }
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "not a comparison expression: {:?}",
                expr
            ))),
        }
    }
}
