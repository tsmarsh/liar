//! liar compiler - borrow-checked Lisp to lIR
//!
//! # Pipeline
//!
//! ```text
//! Source → Lexer → Tokens
//!               ↓
//!          Parser → AST
//!               ↓
//!          Resolve → AST with resolved names
//!               ↓
//!          Infer → Typed AST
//!               ↓
//!          Ownership → Verified AST (borrow-checked)
//!               ↓
//!          Closures → AST with closure info
//!               ↓
//!          Codegen → lIR
//! ```

pub mod ast;
pub mod closures;
pub mod codegen;
pub mod error;
pub mod infer;
pub mod lexer;
pub mod ownership;
pub mod parser;
pub mod resolve;
pub mod span;
pub mod types;

use error::{CompileError, Result};
use parser::Parser;
use types::TypeEnv;

/// Compile liar source to lIR string
pub fn compile(source: &str) -> std::result::Result<String, Vec<CompileError>> {
    // Parse
    let mut parser = Parser::new(source).map_err(|e| vec![e])?;
    let mut program = parser.parse_program().map_err(|e| vec![e])?;

    // Resolve names
    resolve::resolve(&program).map_err(|e| vec![e])?;

    // Type inference
    let mut type_env = TypeEnv::new();
    infer::infer(&program, &mut type_env).map_err(|e| vec![e])?;

    // Ownership checking
    ownership::check(&program).map_err(|e| vec![e])?;

    // Closure analysis
    closures::analyze(&mut program).map_err(|e| vec![e])?;

    // Code generation
    codegen::generate_string(&program).map_err(|e| vec![e])
}

/// Parse and compile a single expression (for REPL)
pub fn compile_expr(source: &str) -> Result<String> {
    let mut parser = Parser::new(source)?;
    let expr = parser.parse_expr()?;
    codegen::generate_expr_standalone(&expr)
}
