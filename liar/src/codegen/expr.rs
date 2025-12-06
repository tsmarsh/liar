//! Expression code generation
//!
//! Main generate_expr function that dispatches to specialized modules.

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::builtins::generate_builtin;
use super::context::CodegenContext;
use super::protocols::generate_protocol_call;
use super::structs::{
    generate_field_access, generate_struct_constructor, is_struct_constructor_call,
};

/// Generate lIR expression from liar expression
pub fn generate_expr(ctx: &mut CodegenContext, expr: &Spanned<Expr>) -> Result<lir::Expr> {
    match &expr.node {
        // Literals
        Expr::Int(n) => Ok(lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: *n as i128,
        }),
        Expr::Float(f) => Ok(lir::Expr::FloatLit {
            ty: lir::ScalarType::Double,
            value: lir::FloatValue::Number(*f),
        }),
        Expr::Bool(b) => Ok(lir::Expr::IntLit {
            ty: lir::ScalarType::I1,
            value: if *b { 1 } else { 0 },
        }),
        Expr::String(s) => Ok(lir::Expr::StringLit(s.clone())),
        Expr::Nil => Ok(lir::Expr::NullPtr),

        // Variables - just emit LocalRef
        // Function references are converted to ClosureLit by closure conversion pass
        Expr::Var(name) => Ok(lir::Expr::LocalRef(name.clone())),

        // Function calls
        Expr::Call(func, args) => generate_call(ctx, expr, func, args),

        // Control flow
        Expr::If(cond, then, else_) => {
            let cond_expr = generate_expr(ctx, cond)?;
            let then_expr = generate_expr(ctx, then)?;
            let else_expr = generate_expr(ctx, else_)?;
            Ok(lir::Expr::Select {
                cond: Box::new(cond_expr),
                true_val: Box::new(then_expr),
                false_val: Box::new(else_expr),
            })
        }

        // Let bindings
        Expr::Let(bindings, body) => {
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

        // Parallel let (same as let for codegen)
        Expr::Plet(bindings, body) => {
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

        // Do block - sequence of expressions
        Expr::Do(exprs) => {
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

        // Lambdas should be converted to ClosureLit by closure conversion pass
        Expr::Lambda(_, _) => {
            unreachable!("Lambda should be converted to ClosureLit before codegen")
        }

        // Mutation
        Expr::Set(name, value) => {
            let value_expr = generate_expr(ctx, value)?;
            Ok(lir::Expr::Store {
                value: Box::new(value_expr),
                ptr: Box::new(lir::Expr::LocalRef(name.node.clone())),
            })
        }

        // References
        Expr::Ref(inner) => {
            let inner_expr = generate_expr(ctx, inner)?;
            Ok(lir::Expr::BorrowRef {
                value: Box::new(inner_expr),
            })
        }
        Expr::RefMut(inner) => {
            let inner_expr = generate_expr(ctx, inner)?;
            Ok(lir::Expr::BorrowRefMut {
                value: Box::new(inner_expr),
            })
        }
        Expr::Deref(inner) => {
            let inner_expr = generate_expr(ctx, inner)?;
            Ok(lir::Expr::Load {
                ty: lir::ParamType::Scalar(lir::ScalarType::I64),
                ptr: Box::new(inner_expr),
            })
        }

        // Structs
        Expr::Struct(name, fields) => {
            let field_values: Result<Vec<lir::Expr>> =
                fields.iter().map(|(_, v)| generate_expr(ctx, v)).collect();
            let _field_values = field_values?;
            // For now, return a placeholder - real struct handling needs GEP
            Ok(lir::Expr::StringLit(format!("struct:{}", name)))
        }
        Expr::Field(obj, field) => generate_field_access(ctx, expr, obj, field),

        // Match
        Expr::Match(_scrutinee, _arms) => Err(CompileError::codegen(
            expr.span,
            "match requires control flow codegen (not yet implemented)",
        )),

        // Quote
        Expr::Quote(sym) => Ok(lir::Expr::StringLit(format!("symbol:{}", sym))),

        // Unsafe
        Expr::Unsafe(inner) => generate_expr(ctx, inner),

        // Atoms (ADR-011)
        Expr::Atom(value) => generate_atom_create(ctx, value),
        Expr::AtomDeref(atom) => generate_atom_deref(ctx, atom),
        Expr::Reset(atom, value) => generate_atom_reset(ctx, atom, value),
        Expr::Swap(atom, func) => generate_atom_swap(ctx, expr, atom, func),
        Expr::CompareAndSet { atom, old, new } => generate_atom_cas(ctx, atom, old, new),

        // Collections (stubs for now - these need runtime support)
        Expr::Vector(elements) => {
            let elems: Result<Vec<lir::Expr>> =
                elements.iter().map(|e| generate_expr(ctx, e)).collect();
            // Placeholder - real implementation needs persistent vector runtime
            Ok(lir::Expr::StructLit(elems?))
        }

        Expr::Map(pairs) => {
            let mut lir_pairs = Vec::new();
            for (k, v) in pairs {
                lir_pairs.push(generate_expr(ctx, k)?);
                lir_pairs.push(generate_expr(ctx, v)?);
            }
            Ok(lir::Expr::StructLit(lir_pairs))
        }

        Expr::Keyword(name) => Ok(lir::Expr::StringLit(format!(":{}", name))),

        Expr::ConvVector(elements) => {
            let elems: Result<Vec<lir::Expr>> =
                elements.iter().map(|e| generate_expr(ctx, e)).collect();
            Ok(lir::Expr::StructLit(elems?))
        }

        Expr::ConvMap(pairs) => {
            let mut lir_pairs = Vec::new();
            for (k, v) in pairs {
                lir_pairs.push(generate_expr(ctx, k)?);
                lir_pairs.push(generate_expr(ctx, v)?);
            }
            Ok(lir::Expr::StructLit(lir_pairs))
        }

        // Async (stubs)
        Expr::Async(body) => {
            let body_expr = generate_expr(ctx, body)?;
            // Placeholder - real async needs runtime support
            Ok(body_expr)
        }
        Expr::Await(future) => generate_expr(ctx, future),

        // SIMD vectors
        Expr::SimdVector(elements) => {
            let elems: Result<Vec<lir::Expr>> =
                elements.iter().map(|e| generate_expr(ctx, e)).collect();
            let elems = elems?;
            let n = elements.len() as u32;

            // Infer element type
            let elem_type = if let Some(first) = elements.first() {
                match &first.node {
                    Expr::Float(_) => lir::ScalarType::Double,
                    _ => lir::ScalarType::I64,
                }
            } else {
                lir::ScalarType::I64
            };

            Ok(lir::Expr::VectorLit {
                ty: lir::VectorType {
                    count: n,
                    element: elem_type,
                },
                elements: elems,
            })
        }

        // STM (stubs)
        Expr::Dosync(exprs) => {
            if exprs.is_empty() {
                return Ok(lir::Expr::NullPtr);
            }
            // Just evaluate the body for now
            if exprs.len() == 1 {
                return generate_expr(ctx, &exprs[0]);
            }
            let mut result = generate_expr(ctx, exprs.last().unwrap())?;
            for (i, e) in exprs.iter().rev().skip(1).enumerate() {
                let value = generate_expr(ctx, e)?;
                result = lir::Expr::Let {
                    bindings: vec![(format!("_stm{}", i), Box::new(value))],
                    body: vec![result],
                };
            }
            Ok(result)
        }

        Expr::RefSetStm(ref_expr, value) => {
            let ref_code = generate_expr(ctx, ref_expr)?;
            let val_code = generate_expr(ctx, value)?;
            Ok(lir::Expr::Store {
                value: Box::new(val_code),
                ptr: Box::new(ref_code),
            })
        }

        Expr::Alter {
            ref_expr,
            fn_expr,
            args,
        } => {
            let ref_code = generate_expr(ctx, ref_expr)?;
            let fn_name = match &fn_expr.node {
                Expr::Var(name) => name.clone(),
                _ => {
                    return Err(CompileError::codegen(
                        fn_expr.span,
                        "alter function must be a variable",
                    ))
                }
            };
            let mut call_args = vec![lir::Expr::Load {
                ty: lir::ParamType::Scalar(lir::ScalarType::I64),
                ptr: Box::new(ref_code.clone()),
            }];
            for arg in args {
                call_args.push(generate_expr(ctx, arg)?);
            }
            let new_val = lir::Expr::Call {
                name: fn_name,
                args: call_args,
            };
            Ok(lir::Expr::Store {
                value: Box::new(new_val),
                ptr: Box::new(ref_code),
            })
        }

        Expr::Commute {
            ref_expr,
            fn_expr,
            args,
        } => {
            // Same as alter for now
            let ref_code = generate_expr(ctx, ref_expr)?;
            let fn_name = match &fn_expr.node {
                Expr::Var(name) => name.clone(),
                _ => {
                    return Err(CompileError::codegen(
                        fn_expr.span,
                        "commute function must be a variable",
                    ))
                }
            };
            let mut call_args = vec![lir::Expr::Load {
                ty: lir::ParamType::Scalar(lir::ScalarType::I64),
                ptr: Box::new(ref_code.clone()),
            }];
            for arg in args {
                call_args.push(generate_expr(ctx, arg)?);
            }
            let new_val = lir::Expr::Call {
                name: fn_name,
                args: call_args,
            };
            Ok(lir::Expr::Store {
                value: Box::new(new_val),
                ptr: Box::new(ref_code),
            })
        }

        // Iterators (stubs)
        Expr::Iter(coll) => generate_expr(ctx, coll),
        Expr::Collect(iter) => generate_expr(ctx, iter),

        // Byte arrays
        Expr::ByteArray(bytes) => {
            let elements: Vec<lir::Expr> = bytes
                .iter()
                .map(|b| lir::Expr::IntLit {
                    ty: lir::ScalarType::I8,
                    value: *b as i128,
                })
                .collect();
            Ok(lir::Expr::StructLit(elements))
        }

        // Regex
        Expr::Regex { pattern, flags } => {
            // Store as string literal for now
            if flags.is_empty() {
                Ok(lir::Expr::StringLit(format!("regex:{}", pattern)))
            } else {
                Ok(lir::Expr::StringLit(format!("regex:{}:{}", pattern, flags)))
            }
        }

        // Overflow handling
        Expr::Boxed(inner) => {
            // For now, just generate the inner expression
            // Real implementation would check for overflow and promote to bigint
            generate_expr(ctx, inner)
        }
        Expr::Wrapping(inner) => {
            // Wrapping arithmetic is the default LLVM behavior
            generate_expr(ctx, inner)
        }

        // Macro syntax - should be expanded before codegen
        Expr::Quasiquote(_) | Expr::Unquote(_) | Expr::UnquoteSplicing(_) | Expr::Gensym(_) => {
            Err(CompileError::codegen(
                expr.span,
                "macro syntax should be expanded before code generation",
            ))
        }

        // Closure conversion output - generated by closure pass
        Expr::ClosureLit { fn_name, env } => {
            let fn_ptr = lir::Expr::GlobalRef(fn_name.clone());
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

        Expr::RcAlloc {
            struct_name,
            fields,
        } => generate_rc_alloc(ctx, struct_name, fields),
    }
}

/// Generate code for creating an atom
fn generate_atom_create(ctx: &mut CodegenContext, value: &Spanned<Expr>) -> Result<lir::Expr> {
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
fn generate_atom_deref(ctx: &mut CodegenContext, atom: &Spanned<Expr>) -> Result<lir::Expr> {
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
fn generate_atom_reset(
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
fn generate_atom_swap(
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
fn generate_atom_cas(
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

/// Generate code for heap allocation (RcAlloc)
fn generate_rc_alloc(
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

/// Generate code for a function call (handles builtins, struct constructors, and protocol methods)
fn generate_call(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Check for builtin operators
    if let Expr::Var(op) = &func.node {
        if let Some(result) = generate_builtin(ctx, expr, op, args)? {
            return Ok(result);
        }

        // Check for struct constructor
        if let Some(struct_info) = ctx.lookup_struct(op).cloned() {
            return generate_struct_constructor(ctx, expr, op, &struct_info, args);
        }

        // Check for protocol method call
        if ctx.is_protocol_method(op).is_some() {
            return generate_protocol_call(ctx, expr, op, args);
        }
    }

    // Get the function name if it's a variable
    if let Expr::Var(name) = &func.node {
        // Check if this is a known function (direct call)
        if ctx.lookup_func_return_type(name).is_some() {
            // Direct function call - prepend null for __env parameter
            // All functions now have __env as first param (uniform calling convention)
            let mut call_args = vec![lir::Expr::NullPtr];
            for arg in args {
                call_args.push(generate_expr(ctx, arg)?);
            }
            return Ok(lir::Expr::Call {
                name: name.clone(),
                args: call_args,
            });
        }

        // Otherwise, treat it as a closure variable
        return generate_closure_call(ctx, name, args);
    }

    // For non-variable function expressions, generate and call as closure
    generate_closure_call_expr(ctx, func, args)
}

/// Generate code for calling a closure stored in a variable
fn generate_closure_call(
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
fn generate_closure_call_expr(
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
