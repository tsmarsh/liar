//! Closure codegen - ClosureLit, RcAlloc, and closure calls
//!
//! Handles the code generation for closure-related AST nodes produced
//! by the closure conversion pass.

use crate::ast::Expr;
use crate::error::Result;
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;

/// Generate code for a ClosureLit expression (closure struct { fn_ptr, env_ptr })
pub fn generate_closure_lit(
    ctx: &mut CodegenContext,
    fn_name: &str,
    env: &Option<Box<Spanned<Expr>>>,
) -> Result<lir::Expr> {
    let fn_ptr = lir::Expr::GlobalRef(fn_name.to_string());
    match env {
        Some(e) => {
            // Generate the env allocation (RcAlloc returns a Let expression)
            let env_expr = generate_expr(ctx, e)?;
            // Bind the env pointer, then create the closure struct
            let env_var = ctx.fresh_var("cenv");
            Ok(lir::Expr::Let {
                bindings: vec![(env_var.clone(), Box::new(env_expr))],
                body: vec![lir::Expr::StructLit(vec![
                    fn_ptr,
                    lir::Expr::LocalRef(env_var),
                ])],
            })
        }
        None => Ok(lir::Expr::StructLit(vec![fn_ptr, lir::Expr::NullPtr])),
    }
}

/// Generate code for heap allocation of closure environment
/// Field 0 is __type_id, user fields start at index 1
pub fn generate_heap_env_alloc(
    ctx: &mut CodegenContext,
    struct_name: &str,
    fields: &[(Spanned<String>, Spanned<Expr>)],
) -> Result<lir::Expr> {
    // Heap-allocate environment struct and store field values
    let env_alloc_name = ctx.fresh_var("env");

    // Mark that we need malloc declaration
    ctx.set_needs_malloc();

    // Calculate size based on field types (includes __type_id at field 0)
    let struct_size = if let Some(struct_info) = ctx.lookup_struct(struct_name) {
        struct_info
            .fields
            .iter()
            .map(|(_, ty)| {
                match ty {
                    lir::ParamType::AnonStruct(inner) => inner.len() * 8, // Each ptr is 8 bytes
                    _ => 8, // Scalars and pointers are 8 bytes
                }
            })
            .sum()
    } else {
        // Fallback: 8 bytes for type_id + 8 bytes per user field
        (fields.len() + 1) * 8
    };

    // Call malloc to allocate the environment struct on the heap
    let malloc_call = lir::Expr::Call {
        name: "malloc".to_string(),
        args: vec![lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: struct_size as i128,
        }],
    };

    let mut stores = vec![(env_alloc_name.clone(), Box::new(malloc_call))];

    // Store type_id at field 0
    let type_id = ctx.get_struct_type_id(struct_name).unwrap_or(0);
    let type_id_gep = lir::Expr::GetElementPtr {
        ty: lir::GepType::Struct(struct_name.to_string()),
        ptr: Box::new(lir::Expr::LocalRef(env_alloc_name.clone())),
        indices: vec![
            lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: 0,
            },
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: 0,
            },
        ],
        inbounds: true,
    };
    stores.push((
        "_store_type_id".to_string(),
        Box::new(lir::Expr::Store {
            value: Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: type_id as i128,
            }),
            ptr: Box::new(type_id_gep),
        }),
    ));

    // Store user field values at indices 1, 2, 3, ...
    for (idx, (_, value)) in fields.iter().enumerate() {
        let gep = lir::Expr::GetElementPtr {
            ty: lir::GepType::Struct(struct_name.to_string()),
            ptr: Box::new(lir::Expr::LocalRef(env_alloc_name.clone())),
            indices: vec![
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I64,
                    value: 0,
                },
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: (idx + 1) as i128, // +1 to skip __type_id
                },
            ],
            inbounds: true,
        };
        let store = lir::Expr::Store {
            value: Box::new(generate_expr(ctx, value)?),
            ptr: Box::new(gep),
        };
        stores.push((format!("_store{}", idx), Box::new(store)));
    }

    // Return the pointer to the allocated struct
    Ok(lir::Expr::Let {
        bindings: stores,
        body: vec![lir::Expr::LocalRef(env_alloc_name)],
    })
}

/// Generate code for stack allocation of closure environment (non-escaping closures)
/// Field 0 is __type_id, user fields start at index 1
pub fn generate_stack_env_alloc(
    ctx: &mut CodegenContext,
    struct_name: &str,
    fields: &[(Spanned<String>, Spanned<Expr>)],
) -> Result<lir::Expr> {
    // Stack-allocate environment struct using alloca and store field values
    let env_alloc_name = ctx.fresh_var("stack_env");

    // Calculate number of fields (includes __type_id)
    let num_fields = if let Some(struct_info) = ctx.lookup_struct(struct_name) {
        struct_info.fields.len()
    } else {
        fields.len() + 1 // +1 for __type_id
    };

    // Use alloca to allocate space on the stack
    let alloca_expr = lir::Expr::Alloca {
        ty: lir::ParamType::Ptr,
        count: if num_fields > 1 {
            Some(Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: num_fields as i128,
            }))
        } else {
            None
        },
    };

    let mut stores = vec![(env_alloc_name.clone(), Box::new(alloca_expr))];

    // Store type_id at field 0
    let type_id = ctx.get_struct_type_id(struct_name).unwrap_or(0);
    let type_id_gep = lir::Expr::GetElementPtr {
        ty: lir::GepType::Struct(struct_name.to_string()),
        ptr: Box::new(lir::Expr::LocalRef(env_alloc_name.clone())),
        indices: vec![
            lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: 0,
            },
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: 0,
            },
        ],
        inbounds: true,
    };
    stores.push((
        "_store_type_id".to_string(),
        Box::new(lir::Expr::Store {
            value: Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: type_id as i128,
            }),
            ptr: Box::new(type_id_gep),
        }),
    ));

    // Store user field values at indices 1, 2, 3, ...
    for (idx, (_, value)) in fields.iter().enumerate() {
        let gep = lir::Expr::GetElementPtr {
            ty: lir::GepType::Struct(struct_name.to_string()),
            ptr: Box::new(lir::Expr::LocalRef(env_alloc_name.clone())),
            indices: vec![
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I64,
                    value: 0,
                },
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: (idx + 1) as i128, // +1 to skip __type_id
                },
            ],
            inbounds: true,
        };
        let store = lir::Expr::Store {
            value: Box::new(generate_expr(ctx, value)?),
            ptr: Box::new(gep),
        };
        stores.push((format!("_store{}", idx), Box::new(store)));
    }

    // Return the pointer to the allocated struct
    Ok(lir::Expr::Let {
        bindings: stores,
        body: vec![lir::Expr::LocalRef(env_alloc_name)],
    })
}

/// Generate code for calling a closure stored in a variable
pub fn generate_closure_call(
    ctx: &mut CodegenContext,
    closure_var: &str,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Closure struct is { fn_ptr: ptr, env_ptr: ptr }
    // Extract fn_ptr (field 0) and env_ptr (field 1)
    let fn_ptr_var = ctx.fresh_var("fn_ptr");
    let env_ptr_var = ctx.fresh_var("env_ptr");

    // Save tail position state before generating args (args are not in tail position)
    let was_tail = ctx.set_tail_position(false);

    // Generate the arguments
    let mut call_args = Vec::new();
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }

    // Restore tail position state
    ctx.set_tail_position(was_tail);

    // Get the closure's return type (from type inference) or default to i64
    let ret_ty = ctx
        .lookup_closure_return_type(closure_var)
        .map(|rt| match rt {
            lir::ReturnType::Scalar(lir::ScalarType::Void) => {
                // Void closures shouldn't happen, but default to i64 if they do
                lir::ParamType::Scalar(lir::ScalarType::I64)
            }
            lir::ReturnType::Scalar(s) => lir::ParamType::Scalar(s.clone()),
            lir::ReturnType::Ptr => lir::ParamType::Ptr,
            lir::ReturnType::AnonStruct(fields) => lir::ParamType::AnonStruct(fields.clone()),
        })
        .unwrap_or(lir::ParamType::Scalar(lir::ScalarType::I64));

    // Build the call expression - use IndirectTailCall if in tail position
    let call_expr = if was_tail && ctx.can_emit_tailcall() {
        lir::Expr::IndirectTailCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var.clone())),
            ret_ty,
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var.clone()))
                .chain(call_args)
                .collect(),
        }
    } else {
        lir::Expr::IndirectCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var.clone())),
            ret_ty,
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var.clone()))
                .chain(call_args)
                .collect(),
        }
    };

    // Build let bindings to extract fn_ptr and env_ptr, then call
    Ok(lir::Expr::Let {
        bindings: vec![
            (
                fn_ptr_var,
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.to_string())),
                    indices: vec![0],
                }),
            ),
            (
                env_ptr_var,
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.to_string())),
                    indices: vec![1],
                }),
            ),
        ],
        body: vec![call_expr],
    })
}

/// Generate code for calling a closure expression (not a variable)
pub fn generate_closure_call_expr(
    ctx: &mut CodegenContext,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Save tail position state - closure expr and args are not in tail position
    let was_tail = ctx.set_tail_position(false);

    // Generate the closure expression
    let closure_expr = generate_expr(ctx, func)?;

    // Bind it to a temporary, then call
    let closure_var = ctx.fresh_var("closure");

    // Generate the arguments
    let mut call_args = Vec::new();
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }

    // Restore tail position state
    ctx.set_tail_position(was_tail);

    let fn_ptr_var = ctx.fresh_var("fn_ptr");
    let env_ptr_var = ctx.fresh_var("env_ptr");

    // Build the call expression - use IndirectTailCall if in tail position
    let call_expr = if was_tail && ctx.can_emit_tailcall() {
        lir::Expr::IndirectTailCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var.clone())),
            ret_ty: lir::ParamType::Scalar(lir::ScalarType::I64), // TODO: infer return type
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var.clone()))
                .chain(call_args)
                .collect(),
        }
    } else {
        lir::Expr::IndirectCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var.clone())),
            ret_ty: lir::ParamType::Scalar(lir::ScalarType::I64), // TODO: infer return type
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var.clone()))
                .chain(call_args)
                .collect(),
        }
    };

    Ok(lir::Expr::Let {
        bindings: vec![
            (closure_var.clone(), Box::new(closure_expr)),
            (
                fn_ptr_var,
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.clone())),
                    indices: vec![0],
                }),
            ),
            (
                env_ptr_var,
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var)),
                    indices: vec![1],
                }),
            ),
        ],
        body: vec![call_expr],
    })
}
