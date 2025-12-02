//! Type checker for lIR expressions

use crate::ast::*;
use crate::error::TypeError;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    /// Check an expression and return its result type
    #[allow(clippy::only_used_in_recursion)]
    pub fn check(&self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::IntLit { ty, .. } => Ok(Type::Scalar(ty.clone())),
            Expr::FloatLit { ty, .. } => Ok(Type::Scalar(ty.clone())),
            Expr::VectorLit { ty, elements } => {
                // Verify element count matches
                if elements.len() != ty.count as usize {
                    return Err(TypeError::VectorCountMismatch);
                }
                // Verify each element has correct type
                let expected = Type::Scalar(ty.element.clone());
                for elem in elements {
                    let elem_ty = self.check(elem)?;
                    if elem_ty != expected {
                        return Err(TypeError::TypeMismatch);
                    }
                }
                Ok(Type::Vector(ty.clone()))
            }

            // Integer arithmetic
            Expr::Add(lhs, rhs)
            | Expr::Sub(lhs, rhs)
            | Expr::Mul(lhs, rhs)
            | Expr::SDiv(lhs, rhs)
            | Expr::UDiv(lhs, rhs)
            | Expr::SRem(lhs, rhs)
            | Expr::URem(lhs, rhs) => {
                let lhs_ty = self.check(lhs)?;
                let rhs_ty = self.check(rhs)?;
                if !lhs_ty.is_integer() {
                    return Err(TypeError::IntOpOnFloat);
                }
                if lhs_ty != rhs_ty {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(lhs_ty)
            }

            // Float arithmetic
            Expr::FAdd(lhs, rhs)
            | Expr::FSub(lhs, rhs)
            | Expr::FMul(lhs, rhs)
            | Expr::FDiv(lhs, rhs)
            | Expr::FRem(lhs, rhs) => {
                let lhs_ty = self.check(lhs)?;
                let rhs_ty = self.check(rhs)?;
                if !lhs_ty.is_float() {
                    return Err(TypeError::FloatOpOnInt);
                }
                if lhs_ty != rhs_ty {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(lhs_ty)
            }

            // Bitwise operations
            Expr::And(lhs, rhs)
            | Expr::Or(lhs, rhs)
            | Expr::Xor(lhs, rhs)
            | Expr::Shl(lhs, rhs)
            | Expr::LShr(lhs, rhs)
            | Expr::AShr(lhs, rhs) => {
                let lhs_ty = self.check(lhs)?;
                let rhs_ty = self.check(rhs)?;
                if !lhs_ty.is_integer() {
                    return Err(TypeError::BitwiseOpOnFloat);
                }
                if lhs_ty != rhs_ty {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(lhs_ty)
            }

            // Integer comparison
            Expr::ICmp { lhs, rhs, .. } => {
                let lhs_ty = self.check(lhs)?;
                let rhs_ty = self.check(rhs)?;
                if !lhs_ty.is_integer() {
                    return Err(TypeError::IcmpRequiresInt);
                }
                if lhs_ty != rhs_ty {
                    return Err(TypeError::TypeMismatch);
                }
                // Result is i1 (or vector of i1 for vector operands)
                match lhs_ty {
                    Type::Scalar(_) => Ok(Type::Scalar(ScalarType::I1)),
                    Type::Vector(vt) => Ok(Type::Vector(VectorType {
                        count: vt.count,
                        element: ScalarType::I1,
                    })),
                }
            }

            // Float comparison
            Expr::FCmp { lhs, rhs, .. } => {
                let lhs_ty = self.check(lhs)?;
                let rhs_ty = self.check(rhs)?;
                if !lhs_ty.is_float() {
                    return Err(TypeError::FcmpRequiresFloat);
                }
                if lhs_ty != rhs_ty {
                    return Err(TypeError::TypeMismatch);
                }
                // Result is i1 (or vector of i1 for vector operands)
                match lhs_ty {
                    Type::Scalar(_) => Ok(Type::Scalar(ScalarType::I1)),
                    Type::Vector(vt) => Ok(Type::Vector(VectorType {
                        count: vt.count,
                        element: ScalarType::I1,
                    })),
                }
            }

            // Integer size conversions
            Expr::Trunc { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_integer() || !ty.is_integer() {
                    return Err(TypeError::TypeMismatch);
                }
                if ty.bit_width() >= src_ty.bit_width() {
                    return Err(TypeError::CannotTruncToLarger);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            Expr::ZExt { ty, value } | Expr::SExt { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_integer() || !ty.is_integer() {
                    return Err(TypeError::TypeMismatch);
                }
                if ty.bit_width() <= src_ty.bit_width() {
                    return Err(TypeError::CannotExtendToSmaller);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            // Float size conversions
            Expr::FPTrunc { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_float() || !ty.is_float() {
                    return Err(TypeError::FptruncRequiresFloat);
                }
                if ty.bit_width() >= src_ty.bit_width() {
                    return Err(TypeError::CannotTruncToLarger);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            Expr::FPExt { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_float() || !ty.is_float() {
                    return Err(TypeError::FpextRequiresFloat);
                }
                if ty.bit_width() <= src_ty.bit_width() {
                    return Err(TypeError::CannotExtendToSmaller);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            // Float to int conversions
            Expr::FPToUI { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_float() {
                    return Err(TypeError::FptouiRequiresFloat);
                }
                if !ty.is_integer() {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            Expr::FPToSI { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_float() {
                    return Err(TypeError::FptosiRequiresFloat);
                }
                if !ty.is_integer() {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            // Int to float conversions
            Expr::UIToFP { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_integer() {
                    return Err(TypeError::UitofpRequiresInt);
                }
                if !ty.is_float() {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            Expr::SIToFP { ty, value } => {
                let src_ty = self.check(value)?;
                if !src_ty.is_integer() {
                    return Err(TypeError::SitofpRequiresInt);
                }
                if !ty.is_float() {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(Type::Scalar(ty.clone()))
            }

            // Select
            Expr::Select {
                cond,
                true_val,
                false_val,
            } => {
                let cond_ty = self.check(cond)?;
                if !cond_ty.is_i1() {
                    return Err(TypeError::ConditionMustBeI1);
                }
                let true_ty = self.check(true_val)?;
                let false_ty = self.check(false_val)?;
                if true_ty != false_ty {
                    return Err(TypeError::TypeMismatch);
                }
                Ok(true_ty)
            }

            // Vector operations
            Expr::ExtractElement { vec, idx } => {
                let vec_ty = self.check(vec)?;
                let idx_ty = self.check(idx)?;

                // Index must be i32
                if idx_ty != Type::Scalar(ScalarType::I32) {
                    return Err(TypeError::VectorIndexMustBeI32);
                }

                match vec_ty {
                    Type::Vector(vt) => Ok(Type::Scalar(vt.element)),
                    _ => Err(TypeError::TypeMismatch),
                }
            }

            Expr::InsertElement { vec, val, idx } => {
                let vec_ty = self.check(vec)?;
                let val_ty = self.check(val)?;
                let idx_ty = self.check(idx)?;

                // Index must be i32
                if idx_ty != Type::Scalar(ScalarType::I32) {
                    return Err(TypeError::VectorIndexMustBeI32);
                }

                match &vec_ty {
                    Type::Vector(vt) => {
                        if val_ty != Type::Scalar(vt.element.clone()) {
                            return Err(TypeError::TypeMismatch);
                        }
                        Ok(vec_ty)
                    }
                    _ => Err(TypeError::TypeMismatch),
                }
            }

            Expr::ShuffleVector { vec1, vec2, mask } => {
                let vec1_ty = self.check(vec1)?;
                let vec2_ty = self.check(vec2)?;
                let mask_ty = self.check(mask)?;

                if vec1_ty != vec2_ty {
                    return Err(TypeError::TypeMismatch);
                }

                match (&vec1_ty, &mask_ty) {
                    (Type::Vector(vt), Type::Vector(mask_vt)) => {
                        // Mask must be integer vector
                        if !mask_vt.element.is_integer() {
                            return Err(TypeError::TypeMismatch);
                        }
                        // Result has same element type as input, but count from mask
                        Ok(Type::Vector(VectorType {
                            count: mask_vt.count,
                            element: vt.element.clone(),
                        }))
                    }
                    _ => Err(TypeError::TypeMismatch),
                }
            }
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn check(input: &str) -> Result<Type, TypeError> {
        let expr = Parser::new(input).parse().unwrap();
        TypeChecker::new().check(&expr)
    }

    #[test]
    fn test_int_literal() {
        assert_eq!(check("(i32 42)").unwrap(), Type::Scalar(ScalarType::I32));
    }

    #[test]
    fn test_add_type_match() {
        assert_eq!(
            check("(add (i32 5) (i32 3))").unwrap(),
            Type::Scalar(ScalarType::I32)
        );
    }

    #[test]
    fn test_add_type_mismatch() {
        assert!(matches!(
            check("(add (i8 1) (i32 2))"),
            Err(TypeError::TypeMismatch)
        ));
    }

    #[test]
    fn test_int_op_on_float() {
        assert!(matches!(
            check("(add (double 1.0) (double 2.0))"),
            Err(TypeError::IntOpOnFloat)
        ));
    }

    #[test]
    fn test_float_op_on_int() {
        assert!(matches!(
            check("(fadd (i32 1) (i32 2))"),
            Err(TypeError::FloatOpOnInt)
        ));
    }

    #[test]
    fn test_icmp_returns_i1() {
        assert_eq!(
            check("(icmp eq (i32 5) (i32 5))").unwrap(),
            Type::Scalar(ScalarType::I1)
        );
    }

    #[test]
    fn test_select_condition_must_be_i1() {
        assert!(matches!(
            check("(select (i32 1) (i32 10) (i32 20))"),
            Err(TypeError::ConditionMustBeI1)
        ));
    }
}
