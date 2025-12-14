//! Code generation - emit lIR AST from liar AST
//!
//! Transforms the liar AST into lIR AST nodes, which can then be:
//! - Displayed as S-expression strings (for debugging/testing)
//! - Passed directly to LLVM codegen (type-safe, no parsing)

mod atoms;
mod builtins;
mod closures_codegen;
mod collections;
mod context;
mod control;
mod expr;
mod protocols;
mod structs;
mod types;

#[cfg(test)]
mod tests;

use crate::ast::{Defun, Extern, Item, Program};
use crate::error::Result;
use crate::span::Spanned;
use crate::types::{Ty, TypeEnv};
use lir_core::ast as lir;
use lir_core::display::Module;

pub use context::{reset_var_counter, CodegenContext, StructInfo};
use expr::generate_expr;
use protocols::generate_extend_protocol;
use structs::generate_defstruct;
use types::{
    infer_function_return_type, infer_liar_expr_type, liar_type_to_lir_param,
    liar_type_to_lir_return, liar_type_to_return,
};

/// Generate lIR module from a liar program
pub fn generate(program: &Program, type_env: &TypeEnv) -> Result<Module> {
    let mut ctx = CodegenContext::new();
    reset_var_counter();

    // First pass: collect struct definitions
    // All structs have a type_id as field 0 for runtime protocol dispatch
    for item in &program.items {
        if let Item::Defstruct(defstruct) = &item.node {
            // Prepend __type_id field (i64) for runtime dispatch
            let mut fields: Vec<(String, lir::ParamType)> = vec![(
                "__type_id".to_string(),
                lir::ParamType::Scalar(lir::ScalarType::I64),
            )];
            fields.extend(defstruct.fields.iter().map(|f| {
                let ty = liar_type_to_lir_param(&f.ty.node);
                (f.name.node.clone(), ty)
            }));
            ctx.register_struct(&defstruct.name.node, StructInfo { fields });
        }
    }

    // Second pass: collect protocol methods
    for item in &program.items {
        if let Item::Defprotocol(protocol) = &item.node {
            for method in &protocol.methods {
                ctx.register_protocol_method(&method.name.node, &protocol.name.node);
            }
        }
    }

    // Third pass: collect function signatures for type inference
    for item in &program.items {
        match &item.node {
            Item::Defun(defun) => {
                let return_type = infer_function_return_type(&ctx, defun);
                ctx.register_func_return_type(&defun.name.node, return_type);
            }
            Item::Extern(ext) => {
                // Register extern function return types so they can be called directly
                let return_type = liar_type_to_lir_return(&ext.return_type.node);
                ctx.register_func_return_type(&ext.name.node, return_type);
                // Mark as extern so we don't prepend __env
                ctx.register_extern(&ext.name.node);
            }
            _ => {}
        }
    }

    // Fourth pass: register all protocol implementations BEFORE generating code
    // This ensures runtime dispatch can find implementations regardless of source order
    for item in &program.items {
        if let Item::ExtendProtocol(extend) = &item.node {
            let protocol_name = &extend.protocol.node;
            let type_name = &extend.type_name.node;

            // Register that this type implements this protocol
            ctx.register_type_protocol(type_name, protocol_name);

            for method_impl in &extend.implementations {
                let method_name = &method_impl.name.node;
                let fn_name = format!("__{}_{}__{}", protocol_name, type_name, method_name);

                // Register this implementation so dispatch can find it
                ctx.register_protocol_impl(type_name, method_name, &fn_name);

                // Infer and register return type
                ctx.start_function();
                ctx.register_var_struct_type("self", type_name);
                let return_type = infer_liar_expr_type(&ctx, &method_impl.body.node);
                ctx.register_func_return_type(&fn_name, return_type);
            }
        }
    }

    // Also register protocol defaults before generating code
    for item in &program.items {
        if let Item::ExtendProtocolDefault(extend) = &item.node {
            let target_protocol = &extend.protocol.node;
            let source_protocol = &extend.source_protocol.node;

            for method_impl in &extend.implementations {
                let method_name = &method_impl.name.node;
                let fn_name = format!("__{}_{}_{}", target_protocol, source_protocol, method_name);

                // Register the default implementation
                ctx.register_protocol_default(
                    target_protocol,
                    source_protocol,
                    method_name,
                    &fn_name,
                );

                // Infer and register return type
                ctx.start_function();
                let return_type = infer_liar_expr_type(&ctx, &method_impl.body.node);
                ctx.register_func_return_type(&fn_name, return_type);
            }
        }
    }

    // Fifth pass: generate code (including protocol implementations)
    let mut items = Vec::new();
    for item in &program.items {
        match &item.node {
            Item::ExtendProtocol(extend) => {
                // Generate functions for each method implementation
                for impl_fn in generate_extend_protocol(&mut ctx, extend)? {
                    items.push(lir::Item::Function(impl_fn));
                }
            }
            Item::ExtendProtocolDefault(extend) => {
                // Generate default implementation functions
                for impl_fn in protocols::generate_extend_protocol_default(&mut ctx, extend)? {
                    items.push(lir::Item::Function(impl_fn));
                }
            }
            _ => {
                if let Some(lir_item) = generate_item(&mut ctx, item, type_env)? {
                    items.push(lir_item);
                }
            }
        }
    }

    // Add malloc declaration if any closures with captures were generated
    if ctx.take_needs_malloc() {
        items.insert(
            0,
            lir::Item::ExternDecl(lir::ExternDecl {
                name: "malloc".to_string(),
                return_type: lir::ReturnType::Ptr,
                param_types: vec![lir::ParamType::Scalar(lir::ScalarType::I64)],
                varargs: false,
            }),
        );
    }

    // Add printf declaration if print/println was used
    if ctx.take_needs_printf() {
        items.insert(
            0,
            lir::Item::ExternDecl(lir::ExternDecl {
                name: "printf".to_string(),
                return_type: lir::ReturnType::Scalar(lir::ScalarType::I32),
                param_types: vec![lir::ParamType::Ptr],
                varargs: true,
            }),
        );
    }

    Ok(Module { items })
}

/// Generate lIR string from a liar program (convenience wrapper)
pub fn generate_string(program: &Program, type_env: &TypeEnv) -> Result<String> {
    let module = generate(program, type_env)?;
    Ok(module.to_string())
}

/// Generate lIR for a standalone expression (for REPL)
pub fn generate_expr_standalone(expr: &Spanned<crate::ast::Expr>) -> Result<String> {
    let mut ctx = CodegenContext::new();
    let lir_expr = generate_expr(&mut ctx, expr)?;
    Ok(lir_expr.to_string())
}

/// Generate lIR for a single item
fn generate_item(
    ctx: &mut CodegenContext,
    item: &Spanned<Item>,
    type_env: &TypeEnv,
) -> Result<Option<lir::Item>> {
    match &item.node {
        Item::Defun(defun) => Ok(Some(lir::Item::Function(generate_defun(
            ctx, defun, type_env,
        )?))),
        Item::Def(_def) => {
            // Global constants need special handling - skip for now
            Ok(None)
        }
        Item::Defstruct(s) => Ok(Some(lir::Item::Struct(generate_defstruct(s)?))),
        Item::Defprotocol(_p) => {
            // Protocols are metadata - skip in lIR output for now
            Ok(None)
        }
        Item::ExtendProtocol(_e) => {
            // Protocol implementations are metadata - skip for now
            Ok(None)
        }
        Item::ExtendProtocolDefault(_e) => {
            // Protocol default implementations are metadata - skip for now
            Ok(None)
        }
        Item::Defmacro(_) => {
            // Macros are used during expansion, not in codegen
            Ok(None)
        }
        Item::Extern(ext) => Ok(Some(lir::Item::ExternDecl(generate_extern(ext)?))),
        Item::Namespace(_) => {
            // Namespace declarations are metadata for module loading
            Ok(None)
        }
    }
}

/// Generate lIR for an external function declaration
fn generate_extern(ext: &Extern) -> Result<lir::ExternDecl> {
    let name = ext.name.node.clone();
    let return_type = liar_type_to_lir_return(&ext.return_type.node);
    let param_types: Vec<lir::ParamType> = ext
        .param_types
        .iter()
        .map(|t| liar_type_to_lir_param(&t.node))
        .collect();

    Ok(lir::ExternDecl {
        name,
        return_type,
        param_types,
        varargs: ext.varargs,
    })
}

/// Generate lIR for a function definition
fn generate_defun(
    ctx: &mut CodegenContext,
    defun: &Defun,
    type_env: &TypeEnv,
) -> Result<lir::FunctionDef> {
    let name = defun.name.node.clone();

    // Initialize block management for this function
    ctx.start_function();

    // Register struct types for parameters
    // First check explicit type annotations, then inferred types from type_env
    for p in &defun.params {
        // Check explicit annotation first
        if let Some(ty) = &p.ty {
            if let crate::ast::Type::Named(struct_name) = &ty.node {
                if ctx.lookup_struct(struct_name).is_some() {
                    ctx.register_var_struct_type(&p.name.node, struct_name);
                    continue;
                }
            }
        }

        // Check type_env for inferred struct types (scoped key: "funcname::paramname")
        let scoped_key = format!("{}::{}", defun.name.node, p.name.node);
        if let Some(Ty::Named(struct_name)) = type_env.get(&scoped_key) {
            if ctx.lookup_struct(struct_name).is_some() {
                ctx.register_var_struct_type(&p.name.node, struct_name);
            }
        }
    }

    // Generate parameters - closure conversion pass already annotates callable params
    let params: Vec<lir::Param> = defun
        .params
        .iter()
        .map(|p| {
            let ty = if p.ty.is_some() {
                liar_type_to_lir_param(&p.ty.as_ref().unwrap().node)
            } else {
                lir::ParamType::Scalar(lir::ScalarType::I64)
            };
            lir::Param {
                ty,
                name: p.name.node.clone(),
            }
        })
        .collect();

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

    // Generate body expression in tail position (for tail call optimization)
    let was_tail = ctx.set_tail_position(true);
    let body_expr = generate_expr(ctx, &defun.body)?;
    ctx.set_tail_position(was_tail);

    // Determine return type - use explicit if provided, otherwise infer from liar body
    // We use the original liar AST for inference since it preserves higher-level type info
    // (e.g., Lambda -> closure struct { ptr, ptr })
    let return_type = defun
        .return_type
        .as_ref()
        .map(|t| liar_type_to_return(&t.node))
        .unwrap_or_else(|| infer_liar_expr_type(ctx, &defun.body.node));

    // Helper to check if an expression ends in a tail call (which is already a terminator)
    // Recursively checks inside Let expressions since Let { ... body: [TailCall] } is also terminal
    fn is_tailcall(expr: &lir::Expr) -> bool {
        match expr {
            lir::Expr::TailCall { .. } => true,
            lir::Expr::Let { body, .. } => body.last().is_some_and(is_tailcall),
            _ => false,
        }
    }

    // Check if any blocks were emitted (from if expressions)
    if ctx.has_blocks() {
        // Multi-block function: collect blocks and add final block with return
        let mut blocks = ctx.take_blocks();

        // Take any pending phis from the current (final) block
        let pending_phis = ctx.take_pending_phis();

        // Wrap the return with any pending phis
        // Note: TailCall is a terminator, so don't wrap it in Ret
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

        // Add the final block (merge block from last if, or entry if no if)
        let final_block = lir::BasicBlock {
            label: ctx.current_block().to_string(),
            instructions: vec![final_instr],
        };
        blocks.push(final_block);

        Ok(lir::FunctionDef {
            name,
            return_type,
            params,
            blocks,
        })
    } else {
        // Single-block function (no if expressions)
        // TailCall is a terminator, so don't wrap it in Ret
        let mut ret_instr = if is_tailcall(&body_expr) {
            body_expr
        } else {
            lir::Expr::Ret(Some(Box::new(body_expr)))
        };

        // Take any entry bindings that weren't consumed by end_block
        // (shouldn't normally happen, but handle gracefully)
        let entry_bindings = ctx.take_entry_bindings();
        if !entry_bindings.is_empty() {
            ret_instr = lir::Expr::Let {
                bindings: entry_bindings,
                body: vec![ret_instr],
            };
        }

        let entry_block = lir::BasicBlock {
            label: "entry".to_string(),
            instructions: vec![ret_instr],
        };

        Ok(lir::FunctionDef {
            name,
            return_type,
            params,
            blocks: vec![entry_block],
        })
    }
}
