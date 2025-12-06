//! Protocol dispatch - generating protocol method implementations
//!
//! Handles extend-protocol and protocol method calls.

use crate::ast::{Expr, ExtendProtocol};
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;
use super::types::infer_return_type;

/// Generate lIR functions for an extend-protocol declaration
/// Each method implementation becomes a function: __<Protocol>_<Type>__<method>
pub fn generate_extend_protocol(
    ctx: &mut CodegenContext,
    extend: &ExtendProtocol,
) -> Result<Vec<lir::FunctionDef>> {
    let protocol_name = &extend.protocol.node;
    let type_name = &extend.type_name.node;

    let mut functions = Vec::new();

    for method_impl in &extend.implementations {
        let method_name = &method_impl.name.node;

        // Generate function name: __Greet_Person__greet
        let fn_name = format!("__{}_{}__{}", protocol_name, type_name, method_name);

        // Register this implementation so we can dispatch to it
        ctx.register_protocol_impl(type_name, method_name, &fn_name);

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

        // Register self's struct type for field access in the body
        ctx.register_var_struct_type("self", type_name);

        // Generate body expression
        let body_expr = generate_expr(ctx, &method_impl.body)?;

        // Infer return type from body (before moving body_expr)
        let return_type = infer_return_type(ctx, &body_expr);

        // Wrap in ret instruction
        let ret_instr = lir::Expr::Ret(Some(Box::new(body_expr)));

        let entry_block = lir::BasicBlock {
            label: "entry".to_string(),
            instructions: vec![ret_instr],
        };

        functions.push(lir::FunctionDef {
            name: fn_name,
            return_type,
            params,
            blocks: vec![entry_block],
        });
    }

    Ok(functions)
}

/// Generate code for a protocol method call
/// Uses static dispatch based on the known type of the first (self) argument
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

    // The first argument is `self` - we need to determine its struct type
    let self_arg = &args[0];
    let type_name = infer_struct_type(ctx, self_arg).ok_or_else(|| {
        CompileError::codegen(
            self_arg.span,
            format!(
                "cannot determine type of receiver for protocol method '{}' - expected struct type",
                method_name
            ),
        )
    })?;

    // Look up the implementation function for this type
    let impl_fn_name = ctx
        .lookup_protocol_impl(&type_name, method_name)
        .cloned()
        .ok_or_else(|| {
            CompileError::codegen(
                expr.span,
                format!(
                    "no implementation of method '{}' for type '{}'",
                    method_name, type_name
                ),
            )
        })?;

    // Generate arguments
    let call_args: Result<Vec<lir::Expr>> = args.iter().map(|a| generate_expr(ctx, a)).collect();

    Ok(lir::Expr::Call {
        name: impl_fn_name,
        args: call_args?,
    })
}

/// Infer the struct type of an expression (for protocol dispatch)
pub fn infer_struct_type(ctx: &CodegenContext, expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        // Direct variable reference - look up in our tracking table
        Expr::Var(name) => ctx.lookup_var_struct_type(name).cloned(),

        // Struct constructor call
        Expr::Call(func, _) => {
            if let Expr::Var(name) = &func.node {
                if ctx.lookup_struct(name).is_some() {
                    return Some(name.clone());
                }
            }
            None
        }

        _ => None,
    }
}
