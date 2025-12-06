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

/// Generate code for heap allocation (RcAlloc)
pub fn generate_rc_alloc(
    ctx: &mut CodegenContext,
    struct_name: &str,
    fields: &[(Spanned<String>, Spanned<Expr>)],
) -> Result<lir::Expr> {
    // Heap-allocate environment struct and store field values
    let env_alloc_name = ctx.fresh_var("env");

    // Mark that we need malloc declaration
    ctx.set_needs_malloc();

    // Calculate size based on field types
    // Look up struct info to get actual field sizes
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
        // Fallback: assume 8 bytes per field
        fields.len() * 8
    };

    // Call malloc to allocate the environment struct on the heap
    let malloc_call = lir::Expr::Call {
        name: "malloc".to_string(),
        args: vec![lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: struct_size as i128,
        }],
    };

    // Store field values into the struct
    let mut stores = vec![(env_alloc_name.clone(), Box::new(malloc_call))];
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
                    value: idx as i128,
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

    // Generate the arguments
    let mut call_args = Vec::new();
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }

    // Build let bindings to extract fn_ptr and env_ptr, then call
    Ok(lir::Expr::Let {
        bindings: vec![
            (
                fn_ptr_var.clone(),
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.to_string())),
                    indices: vec![0],
                }),
            ),
            (
                env_ptr_var.clone(),
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.to_string())),
                    indices: vec![1],
                }),
            ),
        ],
        body: vec![lir::Expr::IndirectCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var)),
            ret_ty: lir::ParamType::Scalar(lir::ScalarType::I64), // TODO: infer return type
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var))
                .chain(call_args)
                .collect(),
        }],
    })
}

/// Generate code for calling a closure expression (not a variable)
pub fn generate_closure_call_expr(
    ctx: &mut CodegenContext,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Generate the closure expression
    let closure_expr = generate_expr(ctx, func)?;

    // Bind it to a temporary, then call
    let closure_var = ctx.fresh_var("closure");

    // Generate the arguments
    let mut call_args = Vec::new();
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }

    let fn_ptr_var = ctx.fresh_var("fn_ptr");
    let env_ptr_var = ctx.fresh_var("env_ptr");

    Ok(lir::Expr::Let {
        bindings: vec![
            (closure_var.clone(), Box::new(closure_expr)),
            (
                fn_ptr_var.clone(),
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var.clone())),
                    indices: vec![0],
                }),
            ),
            (
                env_ptr_var.clone(),
                Box::new(lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(closure_var)),
                    indices: vec![1],
                }),
            ),
        ],
        body: vec![lir::Expr::IndirectCall {
            fn_ptr: Box::new(lir::Expr::LocalRef(fn_ptr_var)),
            ret_ty: lir::ParamType::Scalar(lir::ScalarType::I64), // TODO: infer return type
            args: std::iter::once(lir::Expr::LocalRef(env_ptr_var))
                .chain(call_args)
                .collect(),
        }],
    })
}
