//! Type conversion utilities for codegen
//!
//! Functions to convert liar types to lIR types.

use crate::ast::{Defun, Expr, Type};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;

/// Infer return type for type conversion operations based on first argument (target type)
fn infer_conversion_return_type(args: &[Spanned<Expr>]) -> lir::ReturnType {
    if let Some(first_arg) = args.first() {
        if let Expr::Var(type_name) = &first_arg.node {
            return match type_name.name.as_str() {
                "i1" => lir::ReturnType::Scalar(lir::ScalarType::I1),
                "i8" => lir::ReturnType::Scalar(lir::ScalarType::I8),
                "i16" => lir::ReturnType::Scalar(lir::ScalarType::I16),
                "i32" => lir::ReturnType::Scalar(lir::ScalarType::I32),
                "i64" => lir::ReturnType::Scalar(lir::ScalarType::I64),
                "float" => lir::ReturnType::Scalar(lir::ScalarType::Float),
                "double" => lir::ReturnType::Scalar(lir::ScalarType::Double),
                _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
            };
        }
    }
    // Default if we can't determine the type
    lir::ReturnType::Scalar(lir::ScalarType::I64)
}

/// Infer the return type of a liar function definition
pub fn infer_function_return_type(ctx: &CodegenContext, defun: &Defun) -> lir::ReturnType {
    // If explicit return type is given, use it
    if let Some(ref ty) = defun.return_type {
        return liar_type_to_return(&ty.node);
    }
    // Otherwise infer from body
    infer_liar_expr_type(ctx, &defun.body.node)
}

/// Infer the return type of a liar expression (before lowering to lIR)
pub fn infer_liar_expr_type(ctx: &CodegenContext, expr: &Expr) -> lir::ReturnType {
    match expr {
        // Literals
        Expr::Int(_) => lir::ReturnType::Scalar(lir::ScalarType::I64),
        Expr::Float(_) => lir::ReturnType::Scalar(lir::ScalarType::Double),
        Expr::Bool(_) => lir::ReturnType::Scalar(lir::ScalarType::I1),
        Expr::String(_) => lir::ReturnType::Ptr,
        Expr::Nil => lir::ReturnType::Ptr,

        // Comparison operators return bool
        Expr::Call(func, _args) => {
            if let Expr::Var(op) = &func.node {
                match op.name.as_str() {
                    // Integer comparisons return i1
                    "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Float comparisons return i1
                    "=." | "!=." | "<." | ">." | "<=." | ">=." | "f=" | "f!=" | "f<" | "f>"
                    | "f<=" | "f>=" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Float arithmetic returns double
                    "+." | "-." | "*." | "/." | "%." | "fadd" | "fsub" | "fmul" | "fdiv"
                    | "frem" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::Double);
                    }
                    // Boolean ops return i1
                    "not" | "and" | "or" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Type conversions - return type based on first argument
                    "trunc" | "zext" | "sext" | "fptrunc" | "fpext" | "fptosi" | "fptoui"
                    | "sitofp" | "uitofp" => {
                        return infer_conversion_return_type(_args);
                    }
                    // Builtins that return pointers
                    "share" | "box" | "rc-new" | "cons" => {
                        return lir::ReturnType::Ptr;
                    }
                    // nil? returns bool
                    "nil?" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Look up user-defined function return types
                    name => {
                        if let Some(ret_type) = ctx.lookup_func_return_type(name) {
                            return ret_type.clone();
                        }
                        // Check if it's a struct constructor
                        if ctx.lookup_struct(name).is_some() {
                            return lir::ReturnType::Ptr;
                        }
                    }
                }
            }
            // Default for unknown calls
            lir::ReturnType::Scalar(lir::ScalarType::I64)
        }

        // If inherits from branches (check true branch)
        Expr::If(_cond, then_branch, _else_branch) => infer_liar_expr_type(ctx, &then_branch.node),

        // Let inherits from body
        Expr::Let(_bindings, body) => infer_liar_expr_type(ctx, &body.node),
        Expr::Plet(_bindings, body) => infer_liar_expr_type(ctx, &body.node),

        // Do block inherits from last expression
        Expr::Do(exprs) => {
            if let Some(last) = exprs.last() {
                infer_liar_expr_type(ctx, &last.node)
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Variable - look up registered type, special case for 'self'
        Expr::Var(name) => {
            if let Some(ty) = ctx.lookup_var_type(&name.name) {
                ty.clone()
            } else if name.name == "self" {
                lir::ReturnType::Ptr
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Lambda returns a closure struct { fn_ptr: ptr, env_ptr: ptr }
        Expr::Lambda(_, _) => {
            lir::ReturnType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr])
        }

        // ClosureLit also returns a closure struct { fn_ptr: ptr, env_ptr: ptr }
        Expr::ClosureLit { .. } => {
            lir::ReturnType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr])
        }

        // Field access - look up the struct and field type
        Expr::Field(obj, field_name) => {
            // Try to infer the struct type from the object
            if let Expr::Var(var_name) = &obj.node {
                if let Some(struct_name) = ctx.lookup_var_struct_type(&var_name.name) {
                    if let Some(struct_info) = ctx.lookup_struct(struct_name) {
                        for (name, ty) in &struct_info.fields {
                            if name == &field_name.node {
                                // Convert ParamType to ReturnType
                                return match ty {
                                    lir::ParamType::Scalar(s) => lir::ReturnType::Scalar(s.clone()),
                                    lir::ParamType::Ptr
                                    | lir::ParamType::Own(_)
                                    | lir::ParamType::Ref(_)
                                    | lir::ParamType::RefMut(_)
                                    | lir::ParamType::Rc(_) => lir::ReturnType::Ptr,
                                    lir::ParamType::AnonStruct(fields) => {
                                        lir::ReturnType::AnonStruct(fields.clone())
                                    }
                                };
                            }
                        }
                    }
                }
            }
            // Default to i64 if we can't determine the type
            lir::ReturnType::Scalar(lir::ScalarType::I64)
        }

        // Default
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}

/// Convert a liar type to lIR ReturnType (for extern declarations)
pub fn liar_type_to_lir_return(ty: &Type) -> lir::ReturnType {
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ReturnType::Scalar(lir::ScalarType::I8),
            "i16" => lir::ReturnType::Scalar(lir::ScalarType::I16),
            "i32" => lir::ReturnType::Scalar(lir::ScalarType::I32),
            "i64" | "int" => lir::ReturnType::Scalar(lir::ScalarType::I64),
            "float" | "f32" => lir::ReturnType::Scalar(lir::ScalarType::Float),
            "double" | "f64" => lir::ReturnType::Scalar(lir::ScalarType::Double),
            "bool" => lir::ReturnType::Scalar(lir::ScalarType::I1),
            "void" => lir::ReturnType::Scalar(lir::ScalarType::Void),
            "ptr" => lir::ReturnType::Ptr,
            _ => lir::ReturnType::Ptr, // User-defined types are pointers
        },
        Type::Unit => lir::ReturnType::Scalar(lir::ScalarType::Void),
        Type::Ref(_) | Type::RefMut(_) => lir::ReturnType::Ptr,
        Type::Fn(_, _) => lir::ReturnType::Ptr,
        Type::Tuple(_) => lir::ReturnType::Ptr,
        Type::Ptr => lir::ReturnType::Ptr,
        // Closure is { fn_ptr: ptr, env_ptr: ptr }
        Type::Closure => {
            lir::ReturnType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr])
        }
    }
}

/// Convert a liar type to lIR ParamType
pub fn liar_type_to_lir_param(ty: &Type) -> lir::ParamType {
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ParamType::Scalar(lir::ScalarType::I8),
            "i16" => lir::ParamType::Scalar(lir::ScalarType::I16),
            "i32" => lir::ParamType::Scalar(lir::ScalarType::I32),
            "i64" | "int" => lir::ParamType::Scalar(lir::ScalarType::I64),
            "float" | "f32" => lir::ParamType::Scalar(lir::ScalarType::Float),
            "double" | "f64" => lir::ParamType::Scalar(lir::ScalarType::Double),
            "bool" => lir::ParamType::Scalar(lir::ScalarType::I1),
            "ptr" => lir::ParamType::Ptr,
            _ => lir::ParamType::Ptr, // User-defined types are pointers
        },
        Type::Ref(inner) => {
            let inner_scalar = liar_type_to_scalar(inner);
            lir::ParamType::Ref(Box::new(inner_scalar))
        }
        Type::RefMut(inner) => {
            let inner_scalar = liar_type_to_scalar(inner);
            lir::ParamType::RefMut(Box::new(inner_scalar))
        }
        Type::Unit => lir::ParamType::Scalar(lir::ScalarType::Void),
        Type::Fn(_, _) => lir::ParamType::Ptr,
        Type::Tuple(_) => lir::ParamType::Ptr,
        Type::Ptr => lir::ParamType::Ptr,
        // Closure is { fn_ptr: ptr, env_ptr: ptr }
        Type::Closure => lir::ParamType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr]),
    }
}

/// Convert a liar type to lIR ScalarType (for ownership types)
pub fn liar_type_to_scalar(ty: &Type) -> lir::ScalarType {
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ScalarType::I8,
            "i16" => lir::ScalarType::I16,
            "i32" => lir::ScalarType::I32,
            "i64" | "int" => lir::ScalarType::I64,
            "float" | "f32" => lir::ScalarType::Float,
            "double" | "f64" => lir::ScalarType::Double,
            "bool" => lir::ScalarType::I1,
            _ => lir::ScalarType::I64, // Default
        },
        _ => lir::ScalarType::I64,
    }
}

/// Convert liar type to lIR ReturnType (for function definitions)
pub fn liar_type_to_return(ty: &Type) -> lir::ReturnType {
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ReturnType::Scalar(lir::ScalarType::I8),
            "i16" => lir::ReturnType::Scalar(lir::ScalarType::I16),
            "i32" => lir::ReturnType::Scalar(lir::ScalarType::I32),
            "i64" | "int" => lir::ReturnType::Scalar(lir::ScalarType::I64),
            "float" | "f32" => lir::ReturnType::Scalar(lir::ScalarType::Float),
            "double" | "f64" => lir::ReturnType::Scalar(lir::ScalarType::Double),
            "bool" => lir::ReturnType::Scalar(lir::ScalarType::I1),
            "ptr" => lir::ReturnType::Ptr,
            _ => lir::ReturnType::Ptr,
        },
        Type::Unit => lir::ReturnType::Scalar(lir::ScalarType::Void),
        Type::Closure => {
            lir::ReturnType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr])
        }
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}

/// Infer the return type of an already-generated lIR expression
pub fn infer_return_type(ctx: &CodegenContext, expr: &lir::Expr) -> lir::ReturnType {
    match expr {
        // Literals
        lir::Expr::IntLit { ty, .. } => lir::ReturnType::Scalar(ty.clone()),
        lir::Expr::FloatLit { ty, .. } => lir::ReturnType::Scalar(ty.clone()),
        lir::Expr::NullPtr => lir::ReturnType::Ptr,
        lir::Expr::StringLit(_) => lir::ReturnType::Ptr,

        // Comparisons return i1
        lir::Expr::ICmp { .. } => lir::ReturnType::Scalar(lir::ScalarType::I1),
        lir::Expr::FCmp { .. } => lir::ReturnType::Scalar(lir::ScalarType::I1),

        // Boolean ops return i1
        lir::Expr::And(_, _) | lir::Expr::Or(_, _) | lir::Expr::Xor(_, _) => {
            lir::ReturnType::Scalar(lir::ScalarType::I1)
        }

        // Arithmetic inherits from operands (assume i64)
        lir::Expr::Add(a, _)
        | lir::Expr::Sub(a, _)
        | lir::Expr::Mul(a, _)
        | lir::Expr::SDiv(a, _)
        | lir::Expr::UDiv(a, _)
        | lir::Expr::SRem(a, _)
        | lir::Expr::URem(a, _) => infer_return_type(ctx, a),

        // Select inherits from its branches
        lir::Expr::Select { true_val, .. } => infer_return_type(ctx, true_val),

        // Let returns type of body
        lir::Expr::Let { body, .. } => {
            if let Some(last) = body.last() {
                infer_return_type(ctx, last)
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Call - look up from function table
        lir::Expr::Call { name, .. } => ctx
            .lookup_func_return_type(name)
            .cloned()
            .unwrap_or(lir::ReturnType::Scalar(lir::ScalarType::I64)),

        // Local ref - look up registered variable type, special case for 'self'
        lir::Expr::LocalRef(name) => {
            if let Some(ty) = ctx.lookup_var_type(name) {
                ty.clone()
            } else if name == "self" {
                lir::ReturnType::Ptr
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Struct literal - infer types from fields (for closure returns)
        lir::Expr::StructLit(fields) => {
            let field_types: Vec<lir::ParamType> = fields
                .iter()
                .map(|f| match infer_return_type(ctx, f) {
                    lir::ReturnType::Scalar(s) => lir::ParamType::Scalar(s),
                    lir::ReturnType::Ptr => lir::ParamType::Ptr,
                    lir::ReturnType::AnonStruct(_) => lir::ParamType::Ptr, // Nested struct - treat as ptr
                })
                .collect();
            lir::ReturnType::AnonStruct(field_types)
        }

        // Global ref is a function pointer
        lir::Expr::GlobalRef(_) => lir::ReturnType::Ptr,

        // Default to i64
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}
