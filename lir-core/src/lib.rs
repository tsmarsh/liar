//! lIR Core Library
//!
//! Parser, AST, and type checker for lIR expressions.

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod types;

pub use ast::*;
pub use error::{LirError, ParseError, TypeError};
pub use parser::Parser;
pub use types::TypeChecker;
