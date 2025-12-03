//! Name resolution
//!
//! Resolves variable references to their definitions.

use crate::ast::Program;
use crate::error::Result;

/// Resolve names in a program
pub fn resolve(_program: &mut Program) -> Result<()> {
    // TODO: Implement name resolution
    // - Build symbol tables for each scope
    // - Resolve variable references to their definitions
    // - Check for undefined variables
    // - Check for duplicate definitions
    Ok(())
}
