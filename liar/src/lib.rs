//! liar compiler - borrow-checked Lisp to lIR
//!
//! # Pipeline
//!
//! ```text
//! Source → Lexer → Tokens
//!               ↓
//!          Parser → AST
//!               ↓
//!          Expand → AST with macros expanded
//!               ↓
//!          Resolve → AST with resolved names
//!               ↓
//!          Infer → Typed AST
//!               ↓
//!          Ownership → Verified AST (borrow-checked)
//!               ↓
//!          Closures → AST with closure info + converted lambdas
//!               ↓
//!          Codegen → lIR
//! ```

pub mod ast;
pub mod closures;
pub mod codegen;
pub mod error;
pub mod eval;
pub mod expand;
pub mod infer;
pub mod lexer;
pub mod lint;
pub mod loader;
pub mod ownership;
pub mod parser;
pub mod resolve;
pub mod span;
pub mod types;

#[cfg(feature = "jit-macros")]
pub mod macro_jit;

use error::{CompileError, Result};
use parser::Parser;
use types::TypeEnv;

// Re-export Target for external use
pub use ast::Target;

/// Compile liar source to lIR string (uses host target)
pub fn compile(source: &str) -> std::result::Result<String, Vec<CompileError>> {
    compile_with_target(source, Target::host())
}

/// Compile liar source to lIR string for a specific target
pub fn compile_with_target(
    source: &str,
    target: Target,
) -> std::result::Result<String, Vec<CompileError>> {
    compile_inner(source, false, target)
}

/// Compile liar source with optional JIT macro support (uses host target)
///
/// When `use_jit` is true and the jit-macros feature is enabled, macros can
/// call user-defined functions that were defined earlier in the source file.
#[cfg(feature = "jit-macros")]
pub fn compile_with_jit(source: &str) -> std::result::Result<String, Vec<CompileError>> {
    compile_inner(source, true, Target::host())
}

/// Compile liar source with JIT macro support for a specific target
#[cfg(feature = "jit-macros")]
pub fn compile_with_jit_and_target(
    source: &str,
    target: Target,
) -> std::result::Result<String, Vec<CompileError>> {
    compile_inner(source, true, target)
}

fn compile_inner(
    source: &str,
    use_jit: bool,
    target: Target,
) -> std::result::Result<String, Vec<CompileError>> {
    // Parse
    let mut parser = Parser::new(source).map_err(|e| vec![e])?;
    let mut program = parser.parse_program().map_err(|e| vec![e])?;

    // Expand macros (includes when-target filtering based on target)
    // Note: We only use JIT for top-level compilation to avoid infinite recursion
    // (macro_jit uses compile() internally, so it must not trigger JIT again)
    #[cfg(feature = "jit-macros")]
    if use_jit {
        expand::expand_with_source_and_target(&mut program, source, target).map_err(|e| vec![e])?;
    } else {
        expand::expand_with_target(&mut program, target).map_err(|e| vec![e])?;
    }

    #[cfg(not(feature = "jit-macros"))]
    {
        let _ = use_jit; // suppress unused warning
        expand::expand_with_target(&mut program, target).map_err(|e| vec![e])?;
    }

    // Resolve names
    resolve::resolve(&program).map_err(|e| vec![e])?;

    // Type inference
    let mut type_env = TypeEnv::new();
    infer::infer(&program, &mut type_env).map_err(|e| vec![e])?;

    // Ownership checking
    ownership::check(&program).map_err(|e| vec![e])?;

    // Closure analysis - get capture info for each lambda
    let capture_info = closures::analyze(&program).map_err(|e| vec![e])?;

    // Escape analysis - determine which closures can use stack allocation
    let escape_info = closures::EscapeAnalyzer::new().analyze(&program);

    // Closure conversion - transform lambdas into lifted functions
    // Lambdas become ClosureLit nodes with explicit environment structs
    // Non-escaping closures use stack allocation, escaping closures use heap
    // Pass type_env so closures use correct types for captured variables
    let program = closures::convert(program, capture_info, escape_info, type_env.clone())
        .map_err(|e| vec![e])?;

    // Code generation - pass type_env for struct type inference on parameters
    codegen::generate_string(&program, &type_env).map_err(|e| vec![e])
}

/// Parse and compile a single expression (for REPL)
pub fn compile_expr(source: &str) -> Result<String> {
    let mut parser = Parser::new(source)?;
    let expr = parser.parse_expr()?;
    codegen::generate_expr_standalone(&expr)
}

/// Compile a pre-parsed program (uses host target)
///
/// Used by the module loader to compile merged programs.
pub fn compile_program(program: ast::Program) -> std::result::Result<String, Vec<CompileError>> {
    compile_program_with_target(program, Target::host())
}

/// Compile a pre-parsed program for a specific target
pub fn compile_program_with_target(
    mut program: ast::Program,
    target: Target,
) -> std::result::Result<String, Vec<CompileError>> {
    // Expand macros (includes when-target filtering based on target)
    expand::expand_with_target(&mut program, target).map_err(|e| vec![e])?;

    // Resolve names
    resolve::resolve(&program).map_err(|e| vec![e])?;

    // Type inference
    let mut type_env = TypeEnv::new();
    infer::infer(&program, &mut type_env).map_err(|e| vec![e])?;

    // Ownership checking
    ownership::check(&program).map_err(|e| vec![e])?;

    // Closure analysis
    let capture_info = closures::analyze(&program).map_err(|e| vec![e])?;

    // Escape analysis
    let escape_info = closures::EscapeAnalyzer::new().analyze(&program);

    // Closure conversion
    let program = closures::convert(program, capture_info, escape_info, type_env.clone())
        .map_err(|e| vec![e])?;

    // Code generation
    codegen::generate_string(&program, &type_env).map_err(|e| vec![e])
}

// Re-export compile_file from loader
pub use loader::compile_file;
