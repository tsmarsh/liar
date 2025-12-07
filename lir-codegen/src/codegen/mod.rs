//! LLVM IR code generation
//!
//! This module compiles lIR AST to LLVM IR using inkwell bindings.

mod expr;
mod function;
mod memory;
mod types;
mod value;

pub use value::Value;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::PhiValue;
use lir_core::ast::Expr;
use lir_core::error::TypeError;
use std::collections::HashMap;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error("type error: {0}")]
    Type(#[from] TypeError),
    #[error("codegen error: {0}")]
    CodeGen(String),
    #[error("not yet implemented: {0}")]
    NotImplemented(String),
}

pub type Result<T> = std::result::Result<T, CodeGenError>;

/// Deferred phi incoming edges: (phi_node, [(block_label, value_expr), ...])
pub(crate) type DeferredPhis<'ctx> = Vec<(PhiValue<'ctx>, Vec<(String, Box<Expr>)>)>;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    /// Registry of named struct types (name -> LLVM struct type)
    pub(crate) struct_types: HashMap<String, inkwell::types::StructType<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            struct_types: HashMap::new(),
        }
    }

    /// Create a CodeGen with an existing module
    pub fn with_module(context: &'ctx Context, module: Module<'ctx>) -> Self {
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            struct_types: HashMap::new(),
        }
    }
}
