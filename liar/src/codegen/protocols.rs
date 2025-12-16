//! Protocol dispatch - generating protocol method implementations
//!
//! Handles extend-protocol and protocol method calls.

use std::collections::HashSet;

use crate::ast::{Expr, ExtendProtocol, ExtendProtocolDefault, MethodImpl};
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;

/// Find parameters that are used as callables (closures) in a method body
/// Only considers variables that are actual parameters of the method
fn find_closure_params(method: &MethodImpl) -> HashSet<String> {
    // Collect parameter names
    let param_names: HashSet<String> = method.params.iter().map(|p| p.node.clone()).collect();

    // Find which parameters are used in call position
    let mut closure_params = HashSet::new();
    find_callables_in_expr(&method.body.node, &param_names, &mut closure_params);
    closure_params
}

/// Recursively find variables used in call position that are also parameters
fn find_callables_in_expr(
    expr: &Expr,
    param_names: &HashSet<String>,
    closure_params: &mut HashSet<String>,
) {
    match expr {
        Expr::Call(func, args) => {
            // If the function is a variable that's one of our parameters, it's a closure
            if let Expr::Var(name) = &func.node {
                if param_names.contains(&name.name) {
                    closure_params.insert(name.name.clone());
                }
            }
            // Also recurse into function and args
            find_callables_in_expr(&func.node, param_names, closure_params);
            for arg in args {
                find_callables_in_expr(&arg.node, param_names, closure_params);
            }
        }
        Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
            for b in bindings {
                find_callables_in_expr(&b.value.node, param_names, closure_params);
            }
            find_callables_in_expr(&body.node, param_names, closure_params);
        }
        Expr::If(cond, then_, else_) => {
            find_callables_in_expr(&cond.node, param_names, closure_params);
            find_callables_in_expr(&then_.node, param_names, closure_params);
            find_callables_in_expr(&else_.node, param_names, closure_params);
        }
        Expr::Do(exprs) => {
            for e in exprs {
                find_callables_in_expr(&e.node, param_names, closure_params);
            }
        }
        Expr::Lambda(_, body) => {
            find_callables_in_expr(&body.node, param_names, closure_params);
        }
        _ => {}
    }
}

/// Generate lIR functions for an extend-protocol declaration
/// Each method implementation becomes a function: __<Protocol>_<Type>__<method>
pub fn generate_extend_protocol(
    ctx: &mut CodegenContext,
    extend: &ExtendProtocol,
) -> Result<Vec<lir::FunctionDef>> {
    let protocol_name = &extend.protocol.node;
    let type_name = &extend.type_name.node;

    // Register that this type implements this protocol
    // Used for protocol default lookups
    ctx.register_type_protocol(type_name, protocol_name);

    let mut functions = Vec::new();

    for method_impl in &extend.implementations {
        let method_name = &method_impl.name.node;

        // Generate function name: __Greet_Person__greet
        let fn_name = format!("__{}_{}__{}", protocol_name, type_name, method_name);

        // Register this implementation so we can dispatch to it
        ctx.register_protocol_impl(type_name, method_name, &fn_name);

        // We'll register the return type after generating the body

        // Generate parameters - first param is `self` (ptr to struct)
        let params: Vec<lir::Param> = method_impl
            .params
            .iter()
            .map(|p| {
                // `self` is a pointer to the struct
                let ty = if p.node == "self" {
                    lir::ParamType::Ptr
                } else {
                    // Other params default to i64
                    lir::ParamType::Scalar(lir::ScalarType::I64)
                };
                lir::Param {
                    ty,
                    name: p.node.clone(),
                }
            })
            .collect();

        // Initialize block management for this function (like defun does)
        ctx.start_function();

        // Register self's struct type for field access in the body
        ctx.register_var_struct_type("self", type_name);

        // Register parameter types for phi inference
        for param in &params {
            let return_type = match &param.ty {
                lir::ParamType::Ptr
                | lir::ParamType::Own(_)
                | lir::ParamType::Ref(_)
                | lir::ParamType::RefMut(_)
                | lir::ParamType::Rc(_) => lir::ReturnType::Ptr,
                lir::ParamType::Scalar(s) => lir::ReturnType::Scalar(s.clone()),
                lir::ParamType::AnonStruct(fields) => lir::ReturnType::AnonStruct(fields.clone()),
            };
            ctx.register_var_type(&param.name, return_type);
        }

        // Infer return type from original AST (before code generation)
        // This handles cases like `if` where the generated code has phi nodes
        let return_type = super::types::infer_liar_expr_type(ctx, &method_impl.body.node);

        // Register the impl function's return type for dispatch BEFORE generating body
        // so that recursive calls and dispatch can use the correct type
        ctx.register_func_return_type(&fn_name, return_type.clone());

        // Generate body expression
        let body_expr = generate_expr(ctx, &method_impl.body)?;

        // Helper to check if an expression ends in a tail call
        fn is_tailcall(expr: &lir::Expr) -> bool {
            match expr {
                lir::Expr::TailCall { .. } | lir::Expr::IndirectTailCall { .. } => true,
                lir::Expr::Let { body, .. } => body.last().is_some_and(is_tailcall),
                _ => false,
            }
        }

        // Check if any blocks were emitted (from if expressions)
        let blocks = if ctx.has_blocks() {
            // Multi-block function: collect blocks and add final block with return
            let mut blocks = ctx.take_blocks();

            // Take any pending phis from the current (final) block
            let pending_phis = ctx.take_pending_phis();

            // Wrap the return with any pending phis
            let final_instr = if pending_phis.is_empty() {
                if is_tailcall(&body_expr) {
                    body_expr
                } else {
                    lir::Expr::Ret(Some(Box::new(body_expr)))
                }
            } else {
                lir::Expr::Let {
                    bindings: pending_phis,
                    body: vec![if is_tailcall(&body_expr) {
                        body_expr
                    } else {
                        lir::Expr::Ret(Some(Box::new(body_expr)))
                    }],
                }
            };

            // Add the final block
            let final_block = lir::BasicBlock {
                label: ctx.current_block().to_string(),
                instructions: vec![final_instr],
            };
            blocks.push(final_block);

            blocks
        } else {
            // Single-block function (no if expressions)
            let mut ret_instr = if is_tailcall(&body_expr) {
                body_expr
            } else {
                lir::Expr::Ret(Some(Box::new(body_expr)))
            };

            // Take any entry bindings
            let entry_bindings = ctx.take_entry_bindings();
            if !entry_bindings.is_empty() {
                ret_instr = lir::Expr::Let {
                    bindings: entry_bindings,
                    body: vec![ret_instr],
                };
            }

            vec![lir::BasicBlock {
                label: "entry".to_string(),
                instructions: vec![ret_instr],
            }]
        };

        functions.push(lir::FunctionDef {
            name: fn_name,
            return_type,
            params,
            blocks,
        });
    }

    Ok(functions)
}

/// Generate lIR functions for an extend-protocol-default declaration
/// Each method becomes a function: __<TargetProtocol>_<SourceProtocol>__<method>
/// This provides default implementations for any type implementing SourceProtocol
pub fn generate_extend_protocol_default(
    ctx: &mut CodegenContext,
    extend: &ExtendProtocolDefault,
) -> Result<Vec<lir::FunctionDef>> {
    let target_protocol = &extend.protocol.node;
    let source_protocol = &extend.source_protocol.node;

    let mut functions = Vec::new();

    for method_impl in &extend.implementations {
        let method_name = &method_impl.name.node;

        // Generate function name: __Mappable_Seq__map (target_source_method)
        let fn_name = format!("__{}_{}_{}", target_protocol, source_protocol, method_name);

        // Register the default implementation
        ctx.register_protocol_default(target_protocol, source_protocol, method_name, &fn_name);

        // Find parameters used as closures
        let closure_params = find_closure_params(method_impl);

        // Generate parameters - first param is `self` (ptr to struct)
        let params: Vec<lir::Param> = method_impl
            .params
            .iter()
            .map(|p| {
                // `self` is a pointer to the struct
                let ty = if p.node == "self" {
                    lir::ParamType::Ptr
                } else if closure_params.contains(&p.node) {
                    // Parameter is used as a callable - it's a closure struct
                    lir::ParamType::AnonStruct(vec![lir::ParamType::Ptr, lir::ParamType::Ptr])
                } else {
                    // Other params default to i64
                    lir::ParamType::Scalar(lir::ScalarType::I64)
                };
                lir::Param {
                    ty,
                    name: p.node.clone(),
                }
            })
            .collect();

        // Initialize block management for this function
        ctx.start_function();

        // NOTE: We don't register self's struct type here because
        // self could be any type implementing source_protocol
        // The body must use protocol methods (first, rest, etc.) not field access

        // Register parameter types for phi inference
        for param in &params {
            let return_type = match &param.ty {
                lir::ParamType::Ptr
                | lir::ParamType::Own(_)
                | lir::ParamType::Ref(_)
                | lir::ParamType::RefMut(_)
                | lir::ParamType::Rc(_) => lir::ReturnType::Ptr,
                lir::ParamType::Scalar(s) => lir::ReturnType::Scalar(s.clone()),
                lir::ParamType::AnonStruct(fields) => lir::ReturnType::AnonStruct(fields.clone()),
            };
            ctx.register_var_type(&param.name, return_type);
        }

        // Infer return type from AST
        let return_type = super::types::infer_liar_expr_type(ctx, &method_impl.body.node);

        // Register the function's return type BEFORE generating body
        ctx.register_func_return_type(&fn_name, return_type.clone());

        // Generate body expression
        let body_expr = generate_expr(ctx, &method_impl.body)?;

        // Helper to check if an expression ends in a tail call
        fn is_tailcall(expr: &lir::Expr) -> bool {
            match expr {
                lir::Expr::TailCall { .. } | lir::Expr::IndirectTailCall { .. } => true,
                lir::Expr::Let { body, .. } => body.last().is_some_and(is_tailcall),
                _ => false,
            }
        }

        // Check if any blocks were emitted (from if expressions)
        let blocks = if ctx.has_blocks() {
            let mut blocks = ctx.take_blocks();
            let pending_phis = ctx.take_pending_phis();

            let final_instr = if pending_phis.is_empty() {
                if is_tailcall(&body_expr) {
                    body_expr
                } else {
                    lir::Expr::Ret(Some(Box::new(body_expr)))
                }
            } else {
                lir::Expr::Let {
                    bindings: pending_phis,
                    body: vec![if is_tailcall(&body_expr) {
                        body_expr
                    } else {
                        lir::Expr::Ret(Some(Box::new(body_expr)))
                    }],
                }
            };

            let final_block = lir::BasicBlock {
                label: ctx.current_block().to_string(),
                instructions: vec![final_instr],
            };
            blocks.push(final_block);

            blocks
        } else {
            // Single-block function
            let mut ret_instr = if is_tailcall(&body_expr) {
                body_expr
            } else {
                lir::Expr::Ret(Some(Box::new(body_expr)))
            };

            let entry_bindings = ctx.take_entry_bindings();
            if !entry_bindings.is_empty() {
                ret_instr = lir::Expr::Let {
                    bindings: entry_bindings,
                    body: vec![ret_instr],
                };
            }

            vec![lir::BasicBlock {
                label: "entry".to_string(),
                instructions: vec![ret_instr],
            }]
        };

        functions.push(lir::FunctionDef {
            name: fn_name,
            return_type,
            params,
            blocks,
        });
    }

    Ok(functions)
}

/// Generate code for a protocol method call
/// Uses static dispatch if type is known at compile time, otherwise runtime dispatch
pub fn generate_protocol_call(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    method_name: &str,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    if args.is_empty() {
        return Err(CompileError::codegen(
            expr.span,
            format!(
                "protocol method '{}' requires at least one argument (self)",
                method_name
            ),
        ));
    }

    // The first argument is `self` - try to determine its struct type
    let self_arg = &args[0];

    // Generate arguments (we'll need them for either dispatch path)
    // Arguments are NOT in tail position (the protocol call result is what's in tail position)
    let was_tail = ctx.set_tail_position(false);
    let call_args: Result<Vec<lir::Expr>> = args.iter().map(|a| generate_expr(ctx, a)).collect();
    let call_args = call_args?;
    ctx.set_tail_position(was_tail);

    // Try static dispatch first
    if let Some(type_name) = infer_struct_type(ctx, self_arg) {
        // 1. Look up direct implementation for this type
        if let Some(impl_fn_name) = ctx.lookup_protocol_impl(&type_name, method_name).cloned() {
            return Ok(lir::Expr::Call {
                name: impl_fn_name,
                args: call_args,
            });
        }

        // 2. Look up default implementation via type's implemented protocols
        if let Some(default_fn) = ctx.lookup_default_for_type(&type_name, method_name) {
            return Ok(lir::Expr::Call {
                name: default_fn.to_string(),
                args: call_args,
            });
        }
    }

    // Fall back to runtime dispatch
    generate_runtime_dispatch(ctx, expr, method_name, call_args)
}

/// Generate runtime protocol dispatch based on type_id
/// Loads type_id from field 0 of receiver, dispatches via nested selects
/// Handles nil receiver gracefully by returning default value
fn generate_runtime_dispatch(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    method_name: &str,
    call_args: Vec<lir::Expr>,
) -> Result<lir::Expr> {
    // Get all implementations for this method
    let impls = ctx.get_method_implementations(method_name);

    if impls.is_empty() {
        return Err(CompileError::codegen(
            expr.span,
            format!(
                "no implementations found for protocol method '{}'",
                method_name
            ),
        ));
    }

    // The first argument is the receiver (self) - bind it to avoid double evaluation
    let receiver = call_args[0].clone();
    let receiver_var = ctx.fresh_var("recv");
    let type_id_var = ctx.fresh_var("type_id");

    // Load type_id from field 0
    let type_id_ptr = lir::Expr::GetElementPtr {
        ty: lir::GepType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(lir::Expr::LocalRef(receiver_var.clone())),
        indices: vec![lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: 0,
        }],
        inbounds: true,
    };

    let load_type_id = lir::Expr::Load {
        ty: lir::ParamType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(type_id_ptr),
    };

    // Determine return type from first impl to use appropriate default
    let returns_ptr = if let Some((_, _, impl_fn_name)) = impls.first() {
        matches!(
            ctx.lookup_func_return_type(impl_fn_name),
            Some(lir::ReturnType::Ptr)
        )
    } else {
        false
    };

    // Build dispatch chain: nested selects for each implementation
    // Default depends on return type to satisfy LLVM type requirements
    let default_val: lir::Expr = if returns_ptr {
        lir::Expr::NullPtr
    } else {
        lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: 0,
        }
    };

    let mut dispatch_expr = default_val.clone();

    for (type_name, type_id, impl_fn_name) in impls.iter().rev() {
        let cond = lir::Expr::ICmp {
            pred: lir::ICmpPred::Eq,
            lhs: Box::new(lir::Expr::LocalRef(type_id_var.clone())),
            rhs: Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: *type_id as i128,
            }),
        };

        // Build args with bound receiver
        let mut bound_args = call_args.clone();
        bound_args[0] = lir::Expr::LocalRef(receiver_var.clone());

        let impl_call = lir::Expr::Call {
            name: impl_fn_name.clone(),
            args: bound_args,
        };

        dispatch_expr = lir::Expr::Select {
            cond: Box::new(cond),
            true_val: Box::new(impl_call),
            false_val: Box::new(dispatch_expr),
        };

        let _ = type_name;
    }

    // Null check: if receiver is null, return default without loading type_id
    let null_check = lir::Expr::ICmp {
        pred: lir::ICmpPred::Eq,
        lhs: Box::new(lir::Expr::LocalRef(receiver_var.clone())),
        rhs: Box::new(lir::Expr::NullPtr),
    };

    let dispatch_with_type_id = lir::Expr::Let {
        bindings: vec![(type_id_var, Box::new(load_type_id))],
        body: vec![dispatch_expr],
    };

    let null_safe_dispatch = lir::Expr::Select {
        cond: Box::new(null_check),
        true_val: Box::new(default_val),
        false_val: Box::new(dispatch_with_type_id),
    };

    // Wrap in let to bind receiver
    Ok(lir::Expr::Let {
        bindings: vec![(receiver_var, Box::new(receiver))],
        body: vec![null_safe_dispatch],
    })
}

/// Infer the struct type of an expression (for protocol dispatch)
pub fn infer_struct_type(ctx: &CodegenContext, expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        // Direct variable reference - look up in our tracking table
        Expr::Var(name) => ctx.lookup_var_struct_type(&name.name).cloned(),

        // Struct constructor call
        Expr::Call(func, _) => {
            if let Expr::Var(name) = &func.node {
                if ctx.lookup_struct(&name.name).is_some() {
                    return Some(name.name.clone());
                }
            }
            None
        }

        _ => None,
    }
}
