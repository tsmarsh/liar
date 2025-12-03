//! Type representation and operations

use std::collections::HashMap;

/// Internal type representation (for type inference)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// Type variable (for inference)
    Var(TyVar),
    /// Concrete types
    Int,
    Float,
    Bool,
    String,
    Unit,
    /// Reference
    Ref(Box<Ty>),
    /// Mutable reference
    RefMut(Box<Ty>),
    /// Function type
    Fn(Vec<Ty>, Box<Ty>),
    /// Tuple
    Tuple(Vec<Ty>),
    /// Named type (struct, etc.)
    Named(String),
    /// Never type (for diverging expressions)
    Never,
}

/// Type variable ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(pub u32);

/// Type environment
#[derive(Debug, Default)]
pub struct TypeEnv {
    /// Variable types
    pub vars: HashMap<String, Ty>,
    /// Type definitions
    pub types: HashMap<String, TypeDef>,
}

/// Type definition (struct, etc.)
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: String, ty: Ty) {
        self.vars.insert(name, ty);
    }

    pub fn get(&self, name: &str) -> Option<&Ty> {
        self.vars.get(name)
    }
}
