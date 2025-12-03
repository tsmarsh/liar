//! lIR Code Generation
//!
//! LLVM code generation and JIT execution for lIR.

pub mod codegen;
pub mod emit;
pub mod incremental;
pub mod jit;

pub use codegen::{CodeGen, CodeGenError, Value};
pub use emit::{OptLevel, OutputFormat};
pub use incremental::IncrementalJit;
pub use jit::JitEngine;
