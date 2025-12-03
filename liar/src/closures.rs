//! Closure analysis
//!
//! Determines what variables closures capture and how (by value, by ref, by mut ref).
//! Also determines closure "color" (single-threaded vs thread-safe).

use crate::ast::Program;
use crate::error::Result;

/// Closure capture mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    /// Capture by value (move)
    ByValue,
    /// Capture by immutable reference
    ByRef,
    /// Capture by mutable reference
    ByMut,
}

/// Closure "color" for thread safety
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureColor {
    /// Can only be used in single-threaded context (uses `let`)
    SingleThreaded,
    /// Thread-safe (uses only `plet` and atoms)
    ThreadSafe,
}

/// Analyze closures in a program
pub fn analyze(_program: &mut Program) -> Result<()> {
    // TODO: Implement closure analysis
    // - Find all lambda expressions
    // - Determine free variables in each lambda
    // - Determine capture mode for each free variable
    // - Determine closure color based on captured state
    Ok(())
}
