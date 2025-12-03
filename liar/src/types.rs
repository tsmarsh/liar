//! Type representation and operations

use std::collections::HashMap;
use std::fmt;

/// Internal type representation (for type inference)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// Type variable (for inference)
    Var(TyVar),

    // Primitives with specific sizes (lIR types)
    I8,
    I16,
    I32,
    I64,
    Float,  // f32
    Double, // f64
    Bool,
    Char,
    String,
    Unit,
    Ptr,

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
    /// Error placeholder
    Error,
}

impl Ty {
    /// Is this an integer type?
    pub fn is_integer(&self) -> bool {
        matches!(self, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64)
    }

    /// Is this a float type?
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::Float | Ty::Double)
    }

    /// Is this a numeric type?
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Convert to lIR type string
    pub fn to_lir(&self) -> String {
        match self {
            Ty::I8 => "i8".to_string(),
            Ty::I16 => "i16".to_string(),
            Ty::I32 => "i32".to_string(),
            Ty::I64 => "i64".to_string(),
            Ty::Float => "float".to_string(),
            Ty::Double => "double".to_string(),
            Ty::Bool => "i1".to_string(),
            Ty::Char => "i8".to_string(),
            Ty::String => "ptr".to_string(),
            Ty::Unit => "void".to_string(),
            Ty::Ptr => "ptr".to_string(),
            Ty::Ref(_) | Ty::RefMut(_) => "ptr".to_string(),
            Ty::Fn(_, _) => "ptr".to_string(),
            Ty::Tuple(_) => "ptr".to_string(),
            Ty::Named(name) => format!("%struct.{}", name),
            Ty::Never => "void".to_string(),
            Ty::Var(_) => "i64".to_string(), // Default for unresolved
            Ty::Error => "i64".to_string(),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Var(v) => write!(f, "?{}", v.0),
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::Float => write!(f, "float"),
            Ty::Double => write!(f, "double"),
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::String => write!(f, "string"),
            Ty::Unit => write!(f, "()"),
            Ty::Ptr => write!(f, "ptr"),
            Ty::Ref(inner) => write!(f, "&{}", inner),
            Ty::RefMut(inner) => write!(f, "&mut {}", inner),
            Ty::Fn(params, ret) => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Ty::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Ty::Named(name) => write!(f, "{}", name),
            Ty::Never => write!(f, "!"),
            Ty::Error => write!(f, "<error>"),
        }
    }
}

/// Type variable ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(pub u32);

/// Type environment
#[derive(Debug, Default, Clone)]
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

    /// Create a child scope
    pub fn child(&self) -> Self {
        Self {
            vars: self.vars.clone(),
            types: self.types.clone(),
        }
    }
}
