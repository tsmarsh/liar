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
use super::types::infer_return_type;

/// Flatten nested lets that might contain phi nodes.
/// If expr is `Let { bindings: [(name, Phi)], body: [LocalRef(name)] }`,
/// return the inner bindings and a reference to use. Otherwise wrap in a new binding.
fn flatten_for_phi(
    expr: lir::Expr,
    var_name: String,
) -> (Vec<(String, Box<lir::Expr>)>, lir::Expr) {
    if let lir::Expr::Let { bindings, body } = &expr {
        // Check if this is a phi-wrapper pattern: single binding that's a phi,
        // and body is just a reference to that binding
        if bindings.len() == 1 {
            if let lir::Expr::Phi { .. } = bindings[0].1.as_ref() {
                if body.len() == 1 {
                    if let lir::Expr::LocalRef(ref_name) = &body[0] {
                        if ref_name == &bindings[0].0 {
                            // This is a phi wrapper - extract the binding
                            let phi_name = bindings[0].0.clone();
                            let phi_expr = bindings[0].1.clone();
                            return (
                                vec![(phi_name.clone(), phi_expr)],
                                lir::Expr::LocalRef(phi_name),
                            );
                        }
                    }
                }
            }
        }
    }
    // Not a phi wrapper - create a normal binding
    (
        vec![(var_name.clone(), Box::new(expr))],
        lir::Expr::LocalRef(var_name),
    )
}

/// Generate code for if expression (compiles to br/phi for proper short-circuit evaluation)
pub fn generate_if(
    ctx: &mut CodegenContext,
    cond: &Spanned<Expr>,
    then: &Spanned<Expr>,
    else_: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // Save tail position - branches inherit it, but condition does not
    let was_tail = ctx.is_tail_position();
    // Disable tail call emission inside branches - tailcall is a terminator
    // and can't be used inside expressions that need to produce values for phi
    let could_tailcall = ctx.set_can_emit_tailcall(false);

    // Generate the condition expression (NOT in tail position)
    ctx.set_tail_position(false);
    let cond_expr = generate_expr(ctx, cond)?;
    ctx.set_tail_position(was_tail);

    // Create unique labels for the basic blocks
    let then_label = ctx.fresh_block("then");
    let else_label = ctx.fresh_block("else");
    let merge_label = ctx.fresh_block("merge");

    // Variable names for the branch results
    let cond_var = ctx.fresh_var("cond");
    let then_var = ctx.fresh_var("then_val");
    let else_var = ctx.fresh_var("else_val");
    let result_var = ctx.fresh_var("if_result");

    // End current block with conditional branch
    // Flatten cond_expr in case it contains nested phi from inner if
    let (cond_bindings, cond_ref) = flatten_for_phi(cond_expr.clone(), cond_var.clone());
    let mut entry_bindings = cond_bindings;
    // Add the actual condition check
    let final_cond_expr = if let lir::Expr::LocalRef(_) = &cond_ref {
        cond_ref
    } else {
        entry_bindings.push((cond_var.clone(), Box::new(cond_ref)));
        lir::Expr::LocalRef(cond_var.clone())
    };

    // Ensure condition is i1 - if not, compare with 0 for Clojure-style truthiness
    // (anything non-zero is truthy)
    let cond_type = infer_return_type(ctx, &cond_expr);
    let branch_cond = if matches!(cond_type, lir::ReturnType::Scalar(lir::ScalarType::I1)) {
        // Already boolean, use directly
        final_cond_expr
    } else {
        // Non-boolean: compare != 0 for truthiness
        // Bind the value first if not already a LocalRef
        let cond_val = if let lir::Expr::LocalRef(ref name) = final_cond_expr {
            lir::Expr::LocalRef(name.clone())
        } else {
            let truthiness_var = ctx.fresh_var("truthy");
            entry_bindings.push((truthiness_var.clone(), Box::new(final_cond_expr)));
            lir::Expr::LocalRef(truthiness_var)
        };
        // Compare with 0: value != 0
        lir::Expr::ICmp {
            pred: lir::ICmpPred::Ne,
            lhs: Box::new(cond_val),
            rhs: Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: 0,
            }),
        }
    };

    ctx.end_block(lir::Expr::Let {
        bindings: entry_bindings,
        body: vec![lir::Expr::Br(lir::BranchTarget::Conditional {
            cond: Box::new(branch_cond),
            true_label: then_label.clone(),
            false_label: else_label.clone(),
        })],
    });

    // Then block: evaluate then expression, branch to merge
    ctx.start_block(&then_label);
    let then_expr = generate_expr(ctx, then)?;
    // Infer phi type from then branch before flattening
    let phi_type = match infer_return_type(ctx, &then_expr) {
        lir::ReturnType::Scalar(s) => lir::ParamType::Scalar(s),
        lir::ReturnType::Ptr => lir::ParamType::Ptr,
        lir::ReturnType::AnonStruct(fields) => lir::ParamType::AnonStruct(fields),
    };
    // After generating then expr, we might be in a different block (if nested if)
    let then_final_block = ctx.current_block().to_string();
    let (then_bindings, then_ref) = flatten_for_phi(then_expr, then_var.clone());
    // Get the variable name to use in phi
    let then_phi_var = if let lir::Expr::LocalRef(name) = &then_ref {
        name.clone()
    } else {
        then_var.clone()
    };
    ctx.end_block(lir::Expr::Let {
        bindings: then_bindings,
        body: vec![lir::Expr::Br(lir::BranchTarget::Unconditional(
            merge_label.clone(),
        ))],
    });

    // Else block: evaluate else expression, branch to merge
    ctx.start_block(&else_label);
    let else_expr = generate_expr(ctx, else_)?;
    // After generating else expr, we might be in a different block (if nested if)
    let else_final_block = ctx.current_block().to_string();
    let (else_bindings, else_ref) = flatten_for_phi(else_expr, else_var.clone());
    // Get the variable name to use in phi
    let else_phi_var = if let lir::Expr::LocalRef(name) = &else_ref {
        name.clone()
    } else {
        else_var.clone()
    };
    ctx.end_block(lir::Expr::Let {
        bindings: else_bindings,
        body: vec![lir::Expr::Br(lir::BranchTarget::Unconditional(
            merge_label.clone(),
        ))],
    });

    // Start merge block - phi node selects the result
    ctx.start_block(&merge_label);

    // Add the phi as a pending binding - it will be emitted when this block is ended
    // This ensures the phi stays in the correct merge block even if more if expressions follow
    let phi_expr = lir::Expr::Phi {
        ty: phi_type,
        incoming: vec![
            (
                then_final_block,
                Box::new(lir::Expr::LocalRef(then_phi_var)),
            ),
            (
                else_final_block,
                Box::new(lir::Expr::LocalRef(else_phi_var)),
            ),
        ],
    };
    ctx.add_pending_phi(result_var.clone(), phi_expr);

    // Restore tail call capability
    ctx.set_can_emit_tailcall(could_tailcall);

    // Return just a reference to the phi variable
    Ok(lir::Expr::LocalRef(result_var))
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

/// Check if an expression will generate multi-block code (if expression, instance?, etc.)
/// This recursively checks all subexpressions because any multiblock code generation
/// within a let body means bindings must be emitted before the branches.
fn will_generate_multiblock(expr: &Expr) -> bool {
    match expr {
        Expr::If(_, _, _) => true,
        Expr::Instance(_, _) => true,
        Expr::Let(_, body) | Expr::Plet(_, body) => will_generate_multiblock(&body.node),
        Expr::Do(exprs) => exprs.iter().any(|e| will_generate_multiblock(&e.node)),
        Expr::Call(func, args) => {
            will_generate_multiblock(&func.node)
                || args.iter().any(|a| will_generate_multiblock(&a.node))
        }
        Expr::Field(obj, _) => will_generate_multiblock(&obj.node),
        Expr::Set(_, value) => will_generate_multiblock(&value.node),
        Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) => {
            will_generate_multiblock(&inner.node)
        }
        _ => false,
    }
}

/// Generate code for let bindings
pub fn generate_let(
    ctx: &mut CodegenContext,
    bindings: &[LetBinding],
    body: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // Save tail position - body inherits it, but bindings do not
    let was_tail = ctx.is_tail_position();

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

    // Check if body OR any binding value will create multi-block code (if expression)
    // If so, we need to emit bindings BEFORE branches are generated
    // This is critical when a binding value contains an if that uses an earlier binding
    let body_is_multiblock = will_generate_multiblock(&body.node);
    let any_binding_multiblock = bindings
        .iter()
        .any(|b| will_generate_multiblock(&b.value.node));

    if body_is_multiblock || any_binding_multiblock {
        // Multi-block code path: bindings need to be emitted carefully
        // Each binding might create blocks (if expressions), so we track which block
        // we're in AFTER generating each binding value
        ctx.set_tail_position(false);
        for binding in bindings.iter() {
            // Track which block we're in before generating the value
            let block_before = ctx.current_block().to_string();

            let value = generate_expr(ctx, &binding.value)?;

            // Register the variable type so phi nodes can infer correct types
            let var_type = infer_return_type(ctx, &value);
            ctx.register_var_type(&binding.name.node, var_type);

            // Track which block we're in after generating the value
            let block_after = ctx.current_block().to_string();

            // Add binding to the appropriate block
            // If we're still in the same block, add to that block's bindings
            // If we moved to a new block (due to if expression), add to the new block
            if block_after == "entry" {
                ctx.add_entry_binding(binding.name.node.clone(), value);
            } else {
                ctx.add_block_binding(binding.name.node.clone(), value);
            }

            // If the block changed (multiblock binding value), the bindings
            // accumulated so far in the old block need to be in the block_before block
            // This is handled by how generate_if ends blocks properly
            let _ = block_before; // Suppress unused warning
        }
        ctx.set_tail_position(was_tail);

        // Now generate the body - branches will pick up the bindings
        let body_expr = generate_expr(ctx, body)?;

        // Handle cleanup if needed
        if bindings_needing_cleanup.is_empty() {
            return Ok(body_expr);
        } else {
            let result_var = ctx.fresh_var("let_result");
            let cleanup = generate_cleanup(&bindings_needing_cleanup);
            return Ok(lir::Expr::Let {
                bindings: vec![(result_var.clone(), Box::new(body_expr))],
                body: vec![cleanup, lir::Expr::LocalRef(result_var)],
            });
        }
    }

    // Standard case: body doesn't create multi-block code
    // Body IS in tail position (inherited)
    let body_expr = generate_expr(ctx, body)?;

    // If there are bindings needing cleanup, wrap the body to save result and cleanup
    // NOTE: cleanup breaks tail position since result must be saved first
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
    // Binding values are NOT in tail position
    ctx.set_tail_position(false);
    let mut result = result_with_cleanup;
    for binding in bindings.iter().rev() {
        let value = generate_expr(ctx, &binding.value)?;
        result = lir::Expr::Let {
            bindings: vec![(binding.name.node.clone(), Box::new(value))],
            body: vec![result],
        };
    }
    ctx.set_tail_position(was_tail);
    Ok(result)
}

/// Generate code for parallel let (same as let for codegen)
pub fn generate_plet(
    ctx: &mut CodegenContext,
    bindings: &[LetBinding],
    body: &Spanned<Expr>,
) -> Result<lir::Expr> {
    // Save tail position - body inherits it, but bindings do not
    let was_tail = ctx.is_tail_position();

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

    // Body IS in tail position (inherited)
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

    // Binding values are NOT in tail position
    ctx.set_tail_position(false);
    let mut result = result_with_cleanup;
    for binding in bindings.iter().rev() {
        let value = generate_expr(ctx, &binding.value)?;
        result = lir::Expr::Let {
            bindings: vec![(binding.name.node.clone(), Box::new(value))],
            body: vec![result],
        };
    }
    ctx.set_tail_position(was_tail);
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
        // Single expression inherits tail position
        return generate_expr(ctx, &exprs[0]);
    }

    // Save tail position - only last expression inherits it
    let was_tail = ctx.is_tail_position();

    // Generate as nested lets with dummy bindings
    // Last expression IS in tail position (inherited)
    let mut result = generate_expr(ctx, exprs.last().unwrap())?;

    // All other expressions are NOT in tail position
    ctx.set_tail_position(false);
    for (i, e) in exprs.iter().rev().skip(1).enumerate() {
        let value = generate_expr(ctx, e)?;
        result = lir::Expr::Let {
            bindings: vec![(format!("_discard{}", i), Box::new(value))],
            body: vec![result],
        };
    }
    ctx.set_tail_position(was_tail);
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
