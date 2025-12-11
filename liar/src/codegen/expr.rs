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
        Expr::Var(name) => Ok(lir::Expr::LocalRef(name.clone())),

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
            // Arguments are NOT in tail position
            let was_tail = ctx.set_tail_position(false);
            let mut call_args = vec![lir::Expr::NullPtr];
            for arg in args {
                call_args.push(generate_expr(ctx, arg)?);
            }
            ctx.set_tail_position(was_tail);

            // Use tail call if in tail position AND we can emit tail calls
            // (tail calls are terminators and can't be used inside if branches)
            if was_tail && ctx.can_emit_tailcall() {
                return Ok(lir::Expr::TailCall {
                    name: name.clone(),
                    args: call_args,
                });
            } else {
                return Ok(lir::Expr::Call {
                    name: name.clone(),
                    args: call_args,
                });
            }
        }

        // Otherwise, treat it as a closure variable
        return generate_closure_call(ctx, name, args);
    }

    // For non-variable function expressions, generate and call as closure
    generate_closure_call_expr(ctx, func, args)
}
