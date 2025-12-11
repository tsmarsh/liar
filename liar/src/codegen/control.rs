//! Control flow and binding codegen
//!
//! Handles if, let, plet, do blocks, set!, and unsafe.

use crate::ast::{Expr, LetBinding};
use crate::error::Result;
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

/// Check if an expression is a ClosureLit with a heap-allocated environment
fn has_heap_env(expr: &Expr) -> bool {
    matches!(expr, Expr::ClosureLit { env: Some(_), .. })
}

/// Generate cleanup code for bindings with heap-allocated closure environments
fn generate_cleanup(bindings_needing_cleanup: &[String]) -> lir::Expr {
    // For each closure that needs cleanup, extract env_ptr (field 1) and free it
    // We need to be careful: env_ptr might be null (for closures without captures)
    // So we extract and free unconditionally - free(null) is safe
    let mut cleanup_lets: Vec<(String, Box<lir::Expr>)> = Vec::new();

    for (i, binding_name) in bindings_needing_cleanup.iter().enumerate() {
        // Extract env_ptr from closure struct (field 1)
        let env_ptr_name = format!("__cleanup_env_{}", i);
        let extract_env = lir::Expr::ExtractValue {
            aggregate: Box::new(lir::Expr::LocalRef(binding_name.clone())),
            indices: vec![1], // env_ptr is field 1
        };
        cleanup_lets.push((env_ptr_name.clone(), Box::new(extract_env)));

        // Free the env_ptr
        let free_name = format!("__cleanup_free_{}", i);
        let free_call = lir::Expr::Free {
            ptr: Box::new(lir::Expr::LocalRef(env_ptr_name)),
        };
        cleanup_lets.push((free_name, Box::new(free_call)));
    }

    // Return unit (0) after cleanup
    lir::Expr::Let {
        bindings: cleanup_lets,
        body: vec![lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: 0,
        }],
    }
}

/// Generate code for let bindings
pub fn generate_let(
    ctx: &mut CodegenContext,
    bindings: &[LetBinding],
    body: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // First pass: register struct types for bindings that are struct constructor calls
    // Also track which bindings have heap-allocated closure environments
    let mut bindings_needing_cleanup: Vec<String> = Vec::new();

    for binding in bindings.iter() {
        if let Some(struct_name) = is_struct_constructor_call(ctx, &binding.value.node) {
            ctx.register_var_struct_type(&binding.name.node, &struct_name);
        }
        if has_heap_env(&binding.value.node) {
            bindings_needing_cleanup.push(binding.name.node.clone());
        }
    }

    let body_expr = generate_expr(ctx, body)?;

    // If there are bindings needing cleanup, wrap the body to save result and cleanup
    let result_with_cleanup = if bindings_needing_cleanup.is_empty() {
        body_expr
    } else {
        // Save body result, run cleanup, return saved result
        let result_var = ctx.fresh_var("let_result");
        let cleanup = generate_cleanup(&bindings_needing_cleanup);
        lir::Expr::Let {
            bindings: vec![(result_var.clone(), Box::new(body_expr))],
            body: vec![cleanup, lir::Expr::LocalRef(result_var)],
        }
    };

    // Build let chain from inside out
    let mut result = result_with_cleanup;
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
    // Also track which bindings have heap-allocated closure environments
    let mut bindings_needing_cleanup: Vec<String> = Vec::new();

    for binding in bindings.iter() {
        if let Some(struct_name) = is_struct_constructor_call(ctx, &binding.value.node) {
            ctx.register_var_struct_type(&binding.name.node, &struct_name);
        }
        if has_heap_env(&binding.value.node) {
            bindings_needing_cleanup.push(binding.name.node.clone());
        }
    }

    let body_expr = generate_expr(ctx, body)?;

    // If there are bindings needing cleanup, wrap the body to save result and cleanup
    let result_with_cleanup = if bindings_needing_cleanup.is_empty() {
        body_expr
    } else {
        // Save body result, run cleanup, return saved result
        let result_var = ctx.fresh_var("plet_result");
        let cleanup = generate_cleanup(&bindings_needing_cleanup);
        lir::Expr::Let {
            bindings: vec![(result_var.clone(), Box::new(body_expr))],
            body: vec![cleanup, lir::Expr::LocalRef(result_var)],
        }
    };

    let mut result = result_with_cleanup;
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
