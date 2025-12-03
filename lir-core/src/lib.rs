//! lIR Core Library
//!
//! Parser, AST, type checker, and borrow checker for lIR expressions.

pub mod ast;
pub mod borrow;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod types;

pub use ast::*;
pub use borrow::{BorrowChecker, BorrowError};
pub use error::{LirError, ParseError, TypeError};
pub use parser::{ParseResult, Parser};
pub use types::TypeChecker;
