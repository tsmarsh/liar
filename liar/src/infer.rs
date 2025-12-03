//! Type inference (Hindley-Milner style)

use crate::ast::Program;
use crate::error::Result;
use crate::types::TypeEnv;

/// Infer types for a program
pub fn infer(_program: &mut Program, _env: &mut TypeEnv) -> Result<()> {
    // TODO: Implement Hindley-Milner type inference
    // - Generate type constraints from expressions
    // - Unify constraints
    // - Substitute solved types back into AST
    Ok(())
}
