//! Atom operations codegen
//!
//! Generates lIR for atom creation, deref, reset!, swap!, and compare-and-set!.
//! Atoms compile to LLVM atomic operations (ADR-011).

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;

/// Generate code for creating an atom
pub fn generate_atom_create(ctx: &mut CodegenContext, value: &Spanned<Expr>) -> Result<lir::Expr> {
    let value_expr = generate_expr(ctx, value)?;
    let atom_var = ctx.fresh_var("atom");

    Ok(lir::Expr::Let {
        bindings: vec![(
            atom_var.clone(),
            Box::new(lir::Expr::RcAlloc {
                elem_type: lir::ScalarType::I64,
            }),
        )],
        body: vec![
            lir::Expr::AtomicStore {
                ordering: lir::MemoryOrdering::SeqCst,
                value: Box::new(value_expr),
                ptr: Box::new(lir::Expr::RcPtr {
                    value: Box::new(lir::Expr::LocalRef(atom_var.clone())),
                }),
            },
            lir::Expr::LocalRef(atom_var),
        ],
    })
}

/// Generate code for dereferencing an atom (@a)
pub fn generate_atom_deref(ctx: &mut CodegenContext, atom: &Spanned<Expr>) -> Result<lir::Expr> {
    let atom_expr = generate_expr(ctx, atom)?;
    Ok(lir::Expr::AtomicLoad {
        ordering: lir::MemoryOrdering::SeqCst,
        ty: lir::ScalarType::I64,
        ptr: Box::new(lir::Expr::RcPtr {
            value: Box::new(atom_expr),
        }),
    })
}

/// Generate code for resetting an atom (reset! a v)
pub fn generate_atom_reset(
    ctx: &mut CodegenContext,
    atom: &Spanned<Expr>,
    value: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let atom_expr = generate_expr(ctx, atom)?;
    let value_expr = generate_expr(ctx, value)?;
    let new_var = ctx.fresh_var("new");

    Ok(lir::Expr::Let {
        bindings: vec![(new_var.clone(), Box::new(value_expr))],
        body: vec![
            lir::Expr::AtomicStore {
                ordering: lir::MemoryOrdering::SeqCst,
                value: Box::new(lir::Expr::LocalRef(new_var.clone())),
                ptr: Box::new(lir::Expr::RcPtr {
                    value: Box::new(atom_expr),
                }),
            },
            lir::Expr::LocalRef(new_var),
        ],
    })
}

/// Generate code for swapping an atom (swap! a f)
pub fn generate_atom_swap(
    ctx: &mut CodegenContext,
    _expr: &Spanned<Expr>,
    atom: &Spanned<Expr>,
    func: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let atom_expr = generate_expr(ctx, atom)?;
    let func_name = match &func.node {
        Expr::Var(name) => name.clone(),
        _ => {
            return Err(CompileError::codegen(
                func.span,
                "swap! function must be a variable",
            ))
        }
    };

    let ptr_var = ctx.fresh_var("ptr");
    let old_var = ctx.fresh_var("old");
    let new_var = ctx.fresh_var("new");

    Ok(lir::Expr::Let {
        bindings: vec![(
            ptr_var.clone(),
            Box::new(lir::Expr::RcPtr {
                value: Box::new(atom_expr),
            }),
        )],
        body: vec![lir::Expr::Let {
            bindings: vec![(
                old_var.clone(),
                Box::new(lir::Expr::AtomicLoad {
                    ordering: lir::MemoryOrdering::SeqCst,
                    ty: lir::ScalarType::I64,
                    ptr: Box::new(lir::Expr::LocalRef(ptr_var.clone())),
                }),
            )],
            body: vec![lir::Expr::Let {
                bindings: vec![(
                    new_var.clone(),
                    Box::new(lir::Expr::Call {
                        name: func_name,
                        args: vec![lir::Expr::LocalRef(old_var)],
                    }),
                )],
                body: vec![
                    lir::Expr::AtomicStore {
                        ordering: lir::MemoryOrdering::SeqCst,
                        value: Box::new(lir::Expr::LocalRef(new_var.clone())),
                        ptr: Box::new(lir::Expr::LocalRef(ptr_var)),
                    },
                    lir::Expr::LocalRef(new_var),
                ],
            }],
        }],
    })
}

/// Generate code for compare-and-set (compare-and-set! a old new)
pub fn generate_atom_cas(
    ctx: &mut CodegenContext,
    atom: &Spanned<Expr>,
    old: &Spanned<Expr>,
    new: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let atom_expr = generate_expr(ctx, atom)?;
    let old_expr = generate_expr(ctx, old)?;
    let new_expr = generate_expr(ctx, new)?;
    let result_var = ctx.fresh_var("result");

    Ok(lir::Expr::Let {
        bindings: vec![(
            result_var.clone(),
            Box::new(lir::Expr::CmpXchg {
                ordering: lir::MemoryOrdering::SeqCst,
                ptr: Box::new(lir::Expr::RcPtr {
                    value: Box::new(atom_expr),
                }),
                expected: Box::new(old_expr),
                new_value: Box::new(new_expr),
            }),
        )],
        body: vec![lir::Expr::ExtractValue {
            aggregate: Box::new(lir::Expr::LocalRef(result_var)),
            indices: vec![1],
        }],
    })
}
