//! lIR Code Generation
//!
//! LLVM code generation and JIT execution for lIR.

pub mod codegen;
pub mod emit;
pub mod jit;

pub use codegen::{CodeGen, CodeGenError, Value};
pub use emit::{OptLevel, OutputFormat};
pub use jit::JitEngine;
