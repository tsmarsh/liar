//! Type checker for lIR expressions

use crate::ast::*;
use crate::error::TypeError;
use std::collections::HashMap;

pub struct TypeChecker {
    /// Types of local variables in scope
    locals: HashMap<String, Type>,
    /// Expected return type of the current function
    return_type: Option<ReturnType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            return_type: None,
        }
    }

    /// Create a type checker with function context
    pub fn with_function(params: &[Param], return_type: ReturnType) -> Self {
        let mut locals = HashMap::new();
        for param in params {
            let ty = match &param.ty {
                ParamType::Scalar(s) => Type::Scalar(s.clone()),
                ParamType::Ptr => Type::Ptr,
            };
            locals.insert(param.name.clone(), ty);
        }
        Self {
            locals,
            return_type: Some(return_type),
        }
    }

    /// Check a function definition
    pub fn check_function(&self, func: &FunctionDef) -> Result<(), TypeError> {
        let mut checker = TypeChecker::with_function(&func.params, func.return_type.clone());

        for block in &func.blocks {
            for expr in &block.instructions {
                checker.check(expr)?;
            }
        }

        Ok(())
    }

    /// Check an expression and return its result type
    #[allow(clippy::only_used_in_recursion)]
    pub fn check(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::IntLit { ty, .. } => Ok(Type::Scalar(ty.clone())),
            Expr::FloatLit { ty, .. } => Ok(Type::Scalar(ty.clone())),
            Expr::NullPtr => Ok(Type::Ptr),
            Expr::StringLit(_) => Ok(Type::Ptr), // String literals are pointers to char arrays
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
                    Type::Ptr => Err(TypeError::IcmpRequiresInt), // Already checked above
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
                    Type::Ptr => Err(TypeError::FcmpRequiresFloat), // Already checked above
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

            // Local variable reference
            Expr::LocalRef(name) => self
                .locals
                .get(name)
                .cloned()
                .ok_or(TypeError::UndefinedVariable(name.clone())),

            // Return instruction
            Expr::Ret(value) => {
                if let Some(ret_ty) = self.return_type.clone() {
                    match (&ret_ty, value) {
                        (ReturnType::Scalar(ScalarType::Void), None) => {
                            Ok(Type::Scalar(ScalarType::Void))
                        }
                        (ReturnType::Scalar(ScalarType::Void), Some(_)) => {
                            Err(TypeError::TypeMismatch)
                        }
                        (_, None) => Err(TypeError::TypeMismatch),
                        (ReturnType::Scalar(ty), Some(v)) => {
                            let value_ty = self.check(v)?;
                            if value_ty == Type::Scalar(ty.clone()) {
                                Ok(Type::Scalar(ScalarType::Void)) // ret itself is void
                            } else {
                                Err(TypeError::TypeMismatch)
                            }
                        }
                        (ReturnType::Ptr, Some(v)) => {
                            let value_ty = self.check(v)?;
                            if value_ty == Type::Ptr {
                                Ok(Type::Scalar(ScalarType::Void)) // ret itself is void
                            } else {
                                Err(TypeError::TypeMismatch)
                            }
                        }
                    }
                } else {
                    // No function context - ret not valid
                    Err(TypeError::RetOutsideFunction)
                }
            }

            // Memory operations
            Expr::Alloca { count, .. } => {
                // If there's a count, it must be an integer
                if let Some(c) = count {
                    let count_ty = self.check(c)?;
                    if !count_ty.is_integer() {
                        return Err(TypeError::TypeMismatch);
                    }
                }
                // Alloca returns a pointer
                Ok(Type::Ptr)
            }

            Expr::Load { ty, ptr } => {
                // Pointer must be a pointer type
                let ptr_ty = self.check(ptr)?;
                if !ptr_ty.is_pointer() {
                    return Err(TypeError::LoadRequiresPointer);
                }
                // Load returns the specified type
                match ty {
                    ParamType::Scalar(s) => Ok(Type::Scalar(s.clone())),
                    ParamType::Ptr => Ok(Type::Ptr),
                }
            }

            Expr::Store { value, ptr } => {
                // Pointer must be a pointer type
                let ptr_ty = self.check(ptr)?;
                if !ptr_ty.is_pointer() {
                    return Err(TypeError::StoreRequiresPointer);
                }
                // Check the value type (we don't verify it matches the pointee,
                // that's the programmer's responsibility in lIR)
                self.check(value)?;
                // Store returns void
                Ok(Type::Scalar(ScalarType::Void))
            }

            // Pointer arithmetic
            Expr::GetElementPtr { ptr, indices, .. } => {
                // Pointer must be a pointer type
                let ptr_ty = self.check(ptr)?;
                if !ptr_ty.is_pointer() {
                    return Err(TypeError::GepRequiresPointer);
                }
                // All indices must be integers
                for idx in indices {
                    let idx_ty = self.check(idx)?;
                    if !idx_ty.is_integer() {
                        return Err(TypeError::GepIndexMustBeInt);
                    }
                }
                // GEP returns a pointer
                Ok(Type::Ptr)
            }

            // Control flow
            Expr::Br(target) => {
                // Check conditional branch has i1 condition
                if let BranchTarget::Conditional { cond, .. } = target {
                    let cond_ty = self.check(cond)?;
                    if !cond_ty.is_i1() {
                        return Err(TypeError::ConditionMustBeI1);
                    }
                }
                // Br is a terminator, returns void
                Ok(Type::Scalar(ScalarType::Void))
            }

            Expr::Phi { ty, incoming } => {
                // Check all incoming values have the expected type
                let expected = Type::Scalar(ty.clone());
                for (_, value) in incoming {
                    let value_ty = self.check(value)?;
                    if value_ty != expected {
                        return Err(TypeError::TypeMismatch);
                    }
                }
                Ok(expected)
            }

            Expr::Call { args, .. } => {
                // Type check all arguments
                for arg in args {
                    self.check(arg)?;
                }
                // We can't determine the return type without function context
                // For now, assume i32 - this will be refined when we have a module context
                Ok(Type::Scalar(ScalarType::I32))
            }

            // Struct literal
            Expr::StructLit(fields) => {
                // Check all field types - each field is a valid expression
                for field in fields {
                    self.check(field)?;
                }
                // For now, return i32 as a placeholder. The actual type is determined
                // by the context (extractvalue/insertvalue will look at the fields directly)
                Ok(Type::Scalar(ScalarType::I32))
            }

            // Aggregate operations
            Expr::ExtractValue { aggregate, indices } => {
                // Check aggregate is valid
                self.check(aggregate)?;

                // For struct literals, we can determine the field type directly
                if let Expr::StructLit(fields) = aggregate.as_ref() {
                    if let Some(&idx) = indices.first() {
                        if let Some(field) = fields.get(idx as usize) {
                            return self.check(field);
                        }
                    }
                }

                // For insertvalue chains, recurse to find the innermost struct
                if let Expr::InsertValue {
                    aggregate: inner_agg,
                    value,
                    indices: insert_indices,
                } = aggregate.as_ref()
                {
                    // If extracting from the field that was just inserted, return that field's type
                    if indices == insert_indices {
                        return self.check(value);
                    }
                    // Otherwise, recurse to the inner aggregate
                    return self.check(&Expr::ExtractValue {
                        aggregate: inner_agg.clone(),
                        indices: indices.clone(),
                    });
                }

                // Default fallback - return i32 as placeholder
                Ok(Type::Scalar(ScalarType::I32))
            }

            Expr::InsertValue {
                aggregate, value, ..
            } => {
                // Check both aggregate and value are valid
                self.check(aggregate)?;
                self.check(value)?;
                // Returns the same type as the input aggregate
                self.check(aggregate)
            }

            Expr::Let { bindings, body } => {
                // Save current locals for scoping
                let saved_locals = self.locals.clone();

                // Check all binding expressions and add to locals
                for (name, expr) in bindings {
                    let ty = self.check(expr)?;
                    self.locals.insert(name.clone(), ty);
                }

                // Check all body expressions, return type of last one
                let mut last_type = None;
                for expr in body {
                    last_type = Some(self.check(expr)?);
                }

                // Restore original locals (bindings go out of scope)
                self.locals = saved_locals;

                last_type.ok_or(TypeError::TypeMismatch) // empty body shouldn't happen
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
