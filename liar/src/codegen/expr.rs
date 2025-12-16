//! Expression code generation
//!
//! Main generate_expr function that dispatches to specialized modules.

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::atoms::{
    generate_atom_cas, generate_atom_create, generate_atom_deref, generate_atom_reset,
    generate_atom_swap,
};
use super::builtins::generate_builtin;
use super::closures_codegen::{
    generate_closure_call, generate_closure_call_expr, generate_closure_lit,
    generate_heap_env_alloc, generate_stack_env_alloc,
};
use super::collections::{
    generate_alter, generate_async, generate_await, generate_boxed, generate_byte_array,
    generate_collect, generate_commute, generate_conv_map, generate_conv_vector, generate_dosync,
    generate_iter, generate_keyword, generate_map, generate_quote, generate_ref_set_stm,
    generate_regex, generate_simd_vector, generate_vector, generate_wrapping,
};
use super::context::CodegenContext;
use super::control::{
    generate_deref, generate_do, generate_if, generate_let, generate_plet, generate_ref,
    generate_ref_mut, generate_set, generate_unsafe,
};
use super::protocols::generate_protocol_call;
use super::structs::{generate_field_access, generate_struct_constructor};

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

        // Variables
        Expr::Var(name) => Ok(lir::Expr::LocalRef(name.name.clone())),

        // Function calls
        Expr::Call(func, args) => generate_call(ctx, expr, func, args),

        // Control flow
        Expr::If(cond, then, else_) => generate_if(ctx, cond, then, else_),
        Expr::Let(bindings, body) => generate_let(ctx, bindings, body),
        Expr::Plet(bindings, body) => generate_plet(ctx, bindings, body),
        Expr::Do(exprs) => generate_do(ctx, exprs),
        Expr::Unsafe(inner) => generate_unsafe(ctx, inner),

        // Lambda (should be converted before codegen)
        Expr::Lambda(_, _) => {
            unreachable!("Lambda should be converted to ClosureLit before codegen")
        }

        // Mutation and references
        Expr::Set(name, value) => generate_set(ctx, name, value),
        Expr::Ref(inner) => generate_ref(ctx, inner),
        Expr::RefMut(inner) => generate_ref_mut(ctx, inner),
        Expr::Deref(inner) => generate_deref(ctx, inner),

        // Structs
        Expr::Struct(name, fields) => {
            let field_values: Result<Vec<lir::Expr>> =
                fields.iter().map(|(_, v)| generate_expr(ctx, v)).collect();
            let _field_values = field_values?;
            Ok(lir::Expr::StringLit(format!("struct:{}", name)))
        }
        Expr::Field(obj, field) => generate_field_access(ctx, expr, obj, field),

        // Quote
        Expr::Quote(sym) => Ok(generate_quote(sym)),

        // Atoms (ADR-011)
        Expr::Atom(value) => generate_atom_create(ctx, value),
        Expr::AtomDeref(atom) => generate_atom_deref(ctx, atom),
        Expr::Reset(atom, value) => generate_atom_reset(ctx, atom, value),
        Expr::Swap(atom, func) => generate_atom_swap(ctx, expr, atom, func),
        Expr::CompareAndSet { atom, old, new } => generate_atom_cas(ctx, atom, old, new),

        // Collections
        Expr::Vector(elements) => generate_vector(ctx, elements),
        Expr::Map(pairs) => generate_map(ctx, pairs),
        Expr::Keyword(name) => Ok(generate_keyword(name)),
        Expr::ConvVector(elements) => generate_conv_vector(ctx, elements),
        Expr::ConvMap(pairs) => generate_conv_map(ctx, pairs),
        Expr::SimdVector(elements) => generate_simd_vector(ctx, elements),
        Expr::ByteArray(bytes) => Ok(generate_byte_array(bytes)),
        Expr::Regex { pattern, flags } => Ok(generate_regex(pattern, flags)),

        // Async
        Expr::Async(body) => generate_async(ctx, body),
        Expr::Await(future) => generate_await(ctx, future),

        // STM
        Expr::Dosync(exprs) => generate_dosync(ctx, exprs),
        Expr::RefSetStm(ref_expr, value) => generate_ref_set_stm(ctx, ref_expr, value),
        Expr::Alter {
            ref_expr,
            fn_expr,
            args,
        } => generate_alter(ctx, ref_expr, fn_expr, args),
        Expr::Commute {
            ref_expr,
            fn_expr,
            args,
        } => generate_commute(ctx, ref_expr, fn_expr, args),

        // Iterators
        Expr::Iter(coll) => generate_iter(ctx, coll),
        Expr::Collect(iter) => generate_collect(ctx, iter),

        // Type predicates
        Expr::Instance(obj, type_name) => generate_instance(ctx, obj, type_name),

        // Overflow handling
        Expr::Boxed(inner) => generate_boxed(ctx, inner),
        Expr::Wrapping(inner) => generate_wrapping(ctx, inner),

        // Macro syntax (should be expanded before codegen)
        Expr::Quasiquote(_) | Expr::Unquote(_) | Expr::UnquoteSplicing(_) | Expr::Gensym(_) => {
            Err(CompileError::codegen(
                expr.span,
                "macro syntax should be expanded before code generation",
            ))
        }

        // Closure conversion output
        Expr::ClosureLit { fn_name, env } => generate_closure_lit(ctx, fn_name, env),
        Expr::HeapEnvAlloc {
            struct_name,
            fields,
        } => generate_heap_env_alloc(ctx, struct_name, fields),
        Expr::StackEnvAlloc {
            struct_name,
            fields,
        } => generate_stack_env_alloc(ctx, struct_name, fields),
    }
}

/// Generate code for a function call
fn generate_call(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Check for builtin operators
    if let Expr::Var(op) = &func.node {
        if let Some(result) = generate_builtin(ctx, expr, &op.name, args)? {
            return Ok(result);
        }

        // Check for struct constructor
        if let Some(struct_info) = ctx.lookup_struct(&op.name).cloned() {
            return generate_struct_constructor(ctx, expr, &op.name, &struct_info, args);
        }

        // Check for protocol method call
        if ctx.is_protocol_method(&op.name).is_some() {
            return generate_protocol_call(ctx, expr, &op.name, args);
        }
    }

    // Get the function name if it's a variable
    if let Expr::Var(name) = &func.node {
        // Check if this is a known function (direct call)
        if ctx.lookup_func_return_type(&name.name).is_some() {
            // Arguments are NOT in tail position
            let was_tail = ctx.set_tail_position(false);

            // Extern functions don't take __env parameter
            let mut call_args = if ctx.is_extern(&name.name) {
                Vec::new()
            } else {
                vec![lir::Expr::NullPtr]
            };

            for arg in args {
                call_args.push(generate_expr(ctx, arg)?);
            }
            ctx.set_tail_position(was_tail);

            // Use tail call if in tail position AND we can emit tail calls
            // (tail calls are terminators and can't be used inside if branches)
            if was_tail && ctx.can_emit_tailcall() {
                return Ok(lir::Expr::TailCall {
                    name: name.name.clone(),
                    args: call_args,
                });
            } else {
                return Ok(lir::Expr::Call {
                    name: name.name.clone(),
                    args: call_args,
                });
            }
        }

        // Otherwise, treat it as a closure variable
        return generate_closure_call(ctx, &name.name, args);
    }

    // For non-variable function expressions, generate and call as closure
    generate_closure_call_expr(ctx, func, args)
}

/// Generate code for (instance? obj Type)
/// Returns 1 if obj's runtime type_id matches Type's type_id, 0 otherwise
///
/// For proper null-safety, we synthesize an if expression:
///   (if (nil? obj) 0 (type-check obj))
/// This ensures the type check is only evaluated when obj is not null.
fn generate_instance(
    ctx: &mut CodegenContext,
    obj: &Spanned<Expr>,
    type_name: &Spanned<String>,
) -> Result<lir::Expr> {
    // Get compile-time type_id for the type name
    let expected_type_id = ctx.get_struct_type_id(&type_name.node).ok_or_else(|| {
        CompileError::codegen(
            type_name.span,
            format!("unknown type '{}' in instance? check", type_name.node),
        )
    })?;

    // Generate code for the object expression
    let obj_expr = generate_expr(ctx, obj)?;

    // Bind to a variable to avoid double evaluation
    let obj_var = ctx.fresh_var("instance_obj");

    // Generate nil check
    let nil_check = lir::Expr::ICmp {
        pred: lir::ICmpPred::Eq,
        lhs: Box::new(lir::Expr::LocalRef(obj_var.clone())),
        rhs: Box::new(lir::Expr::NullPtr),
    };

    // Generate type check (for else branch)
    let type_id_var = ctx.fresh_var("type_id");
    let type_id_ptr = lir::Expr::GetElementPtr {
        ty: lir::GepType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(lir::Expr::LocalRef(obj_var.clone())),
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

    let cmp_expr = lir::Expr::ICmp {
        pred: lir::ICmpPred::Eq,
        lhs: Box::new(lir::Expr::LocalRef(type_id_var.clone())),
        rhs: Box::new(lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: expected_type_id as i128,
        }),
    };

    let result = lir::Expr::ZExt {
        ty: lir::ScalarType::I64,
        value: Box::new(cmp_expr),
    };

    let type_check_with_load = lir::Expr::Let {
        bindings: vec![(type_id_var, Box::new(load_type_id))],
        body: vec![result],
    };

    // Generate branching code for null-safety using br/phi pattern
    // Create unique labels for the basic blocks
    let then_label = ctx.fresh_block("instance_null");
    let else_label = ctx.fresh_block("instance_check");
    let merge_label = ctx.fresh_block("instance_merge");

    let then_val_var = ctx.fresh_var("then_val");
    let else_val_var = ctx.fresh_var("else_val");
    let result_var = ctx.fresh_var("if_result");

    // End current block with conditional branch (nil check)
    ctx.end_block(lir::Expr::Let {
        bindings: vec![(obj_var.clone(), Box::new(obj_expr))],
        body: vec![lir::Expr::Br(lir::BranchTarget::Conditional {
            cond: Box::new(nil_check),
            true_label: then_label.clone(),
            false_label: else_label.clone(),
        })],
    });

    // Then block: result is 0
    ctx.start_block(&then_label);
    let then_final_block = ctx.current_block().to_string();
    ctx.end_block(lir::Expr::Let {
        bindings: vec![(
            then_val_var.clone(),
            Box::new(lir::Expr::IntLit {
                ty: lir::ScalarType::I64,
                value: 0,
            }),
        )],
        body: vec![lir::Expr::Br(lir::BranchTarget::Unconditional(
            merge_label.clone(),
        ))],
    });

    // Else block: do type check
    ctx.start_block(&else_label);
    let else_final_block = ctx.current_block().to_string();
    ctx.end_block(lir::Expr::Let {
        bindings: vec![(else_val_var.clone(), Box::new(type_check_with_load))],
        body: vec![lir::Expr::Br(lir::BranchTarget::Unconditional(
            merge_label.clone(),
        ))],
    });

    // Merge block with phi - use add_pending_phi like generate_if does
    ctx.start_block(&merge_label);
    let phi_expr = lir::Expr::Phi {
        ty: lir::ParamType::Scalar(lir::ScalarType::I64),
        incoming: vec![
            (
                then_final_block,
                Box::new(lir::Expr::LocalRef(then_val_var)),
            ),
            (
                else_final_block,
                Box::new(lir::Expr::LocalRef(else_val_var)),
            ),
        ],
    };
    ctx.add_pending_phi(result_var.clone(), phi_expr);

    // Return a reference to the phi result
    Ok(lir::Expr::LocalRef(result_var))
}
