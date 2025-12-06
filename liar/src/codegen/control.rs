//! Control flow and binding codegen
//!
//! Handles if, let, plet, do blocks, set!, match, and unsafe.

use crate::ast::{Expr, LetBinding};
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;
use super::structs::is_struct_constructor_call;

/// Generate code for if expression (compiles to select)
pub fn generate_if(
    ctx: &mut CodegenContext,
    cond: &Spanned<Expr>,
    then: &Spanned<Expr>,
    else_: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let cond_expr = generate_expr(ctx, cond)?;
    let then_expr = generate_expr(ctx, then)?;
    let else_expr = generate_expr(ctx, else_)?;
    Ok(lir::Expr::Select {
        cond: Box::new(cond_expr),
        true_val: Box::new(then_expr),
        false_val: Box::new(else_expr),
    })
}

/// Generate code for let bindings
pub fn generate_let(
    ctx: &mut CodegenContext,
    bindings: &[LetBinding],
    body: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // First pass: register struct types for bindings that are struct constructor calls
    for binding in bindings.iter() {
        if let Some(struct_name) = is_struct_constructor_call(ctx, &binding.value.node) {
            ctx.register_var_struct_type(&binding.name.node, &struct_name);
        }
    }

    let body_expr = generate_expr(ctx, body)?;

    // Build let chain from inside out
    let mut result = body_expr;
    for binding in bindings.iter().rev() {
        let value = generate_expr(ctx, &binding.value)?;
        result = lir::Expr::Let {
            bindings: vec![(binding.name.node.clone(), Box::new(value))],
            body: vec![result],
        };
    }
    Ok(result)
}

/// Generate code for parallel let (same as let for codegen)
pub fn generate_plet(
    ctx: &mut CodegenContext,
    bindings: &[LetBinding],
    body: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // First pass: register struct types for bindings that are struct constructor calls
    for binding in bindings.iter() {
        if let Some(struct_name) = is_struct_constructor_call(ctx, &binding.value.node) {
            ctx.register_var_struct_type(&binding.name.node, &struct_name);
        }
    }

    let body_expr = generate_expr(ctx, body)?;
    let mut result = body_expr;
    for binding in bindings.iter().rev() {
        let value = generate_expr(ctx, &binding.value)?;
        result = lir::Expr::Let {
            bindings: vec![(binding.name.node.clone(), Box::new(value))],
            body: vec![result],
        };
    }
    Ok(result)
}

/// Generate code for do block (sequence of expressions)
pub fn generate_do(ctx: &mut CodegenContext, exprs: &[Spanned<Expr>]) -> Result<lir::Expr> {
    if exprs.is_empty() {
        return Ok(lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: 0,
        });
    }
    if exprs.len() == 1 {
        return generate_expr(ctx, &exprs[0]);
    }

    // Generate as nested lets with dummy bindings
    let mut result = generate_expr(ctx, exprs.last().unwrap())?;
    for (i, e) in exprs.iter().rev().skip(1).enumerate() {
        let value = generate_expr(ctx, e)?;
        result = lir::Expr::Let {
            bindings: vec![(format!("_discard{}", i), Box::new(value))],
            body: vec![result],
        };
    }
    Ok(result)
}

/// Generate code for set! (mutation)
pub fn generate_set(
    ctx: &mut CodegenContext,
    name: &Spanned<String>,
    value: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let value_expr = generate_expr(ctx, value)?;
    Ok(lir::Expr::Store {
        value: Box::new(value_expr),
        ptr: Box::new(lir::Expr::LocalRef(name.node.clone())),
    })
}

/// Generate code for match expression
pub fn generate_match(expr: &Spanned<Expr>) -> Result<lir::Expr> {
    Err(CompileError::codegen(
        expr.span,
        "match requires control flow codegen (not yet implemented)",
    ))
}

/// Generate code for unsafe block (just generates the inner expression)
pub fn generate_unsafe(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    generate_expr(ctx, inner)
}

/// Generate code for reference creation
pub fn generate_ref(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    let inner_expr = generate_expr(ctx, inner)?;
    Ok(lir::Expr::BorrowRef {
        value: Box::new(inner_expr),
    })
}

/// Generate code for mutable reference creation
pub fn generate_ref_mut(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    let inner_expr = generate_expr(ctx, inner)?;
    Ok(lir::Expr::BorrowRefMut {
        value: Box::new(inner_expr),
    })
}

/// Generate code for dereferencing
pub fn generate_deref(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    let inner_expr = generate_expr(ctx, inner)?;
    Ok(lir::Expr::Load {
        ty: lir::ParamType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(inner_expr),
    })
}
