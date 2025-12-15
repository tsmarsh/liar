//! liar Abstract Syntax Tree

use crate::span::Spanned;
use std::fmt;

/// Qualified name: namespace/name or just name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedName {
    /// Namespace qualifier (e.g., "vec" in vec/conj)
    pub qualifier: Option<String>,
    /// Local name (e.g., "conj")
    pub name: String,
}

impl QualifiedName {
    /// Create an unqualified name
    pub fn simple(name: String) -> Self {
        Self {
            qualifier: None,
            name,
        }
    }

    /// Create a qualified name
    pub fn qualified(qualifier: String, name: String) -> Self {
        Self {
            qualifier: Some(qualifier),
            name,
        }
    }

    /// Parse from string (handles "ns/name" or just "name")
    /// Only treats "/" as a namespace separator if both parts are non-empty
    /// and the qualifier looks like an identifier (starts with letter/underscore).
    /// This preserves operators like "/" and "/." as simple names.
    pub fn parse(s: &str) -> Self {
        if let Some(pos) = s.find('/') {
            let qualifier = &s[..pos];
            let name = &s[pos + 1..];
            // Only treat as qualified if:
            // 1. Both parts are non-empty
            // 2. Qualifier starts with a letter or underscore (looks like an identifier)
            if !qualifier.is_empty()
                && !name.is_empty()
                && qualifier
                    .chars()
                    .next()
                    .map(|c| c.is_alphabetic() || c == '_')
                    .unwrap_or(false)
            {
                Self::qualified(qualifier.to_string(), name.to_string())
            } else {
                Self::simple(s.to_string())
            }
        } else {
            Self::simple(s.to_string())
        }
    }

    /// Get the full name as a string
    pub fn full_name(&self) -> String {
        match &self.qualifier {
            Some(q) => format!("{}/{}", q, self.name),
            None => self.name.clone(),
        }
    }

    /// Check if this is an unqualified name
    pub fn is_simple(&self) -> bool {
        self.qualifier.is_none()
    }
}

impl fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.qualifier {
            Some(q) => write!(f, "{}/{}", q, self.name),
            None => write!(f, "{}", self.name),
        }
    }
}

impl PartialEq<str> for QualifiedName {
    fn eq(&self, other: &str) -> bool {
        self.qualifier.is_none() && self.name == other
    }
}

impl PartialEq<&str> for QualifiedName {
    fn eq(&self, other: &&str) -> bool {
        self.qualifier.is_none() && self.name == *other
    }
}

/// Require specification in a namespace declaration
#[derive(Debug, Clone)]
pub enum RequireSpec {
    /// [module :as alias] - require with alias
    Alias {
        module: Spanned<String>,
        alias: Spanned<String>,
    },
    /// [module :refer [sym1 sym2]] - require with specific symbols
    Refer {
        module: Spanned<String>,
        symbols: Vec<Spanned<String>>,
    },
    /// [module :refer :all] - require with all symbols
    ReferAll { module: Spanned<String> },
    /// [module] - bare require (just load, use qualified)
    Bare { module: Spanned<String> },
}

/// Namespace declaration
#[derive(Debug, Clone)]
pub struct Namespace {
    /// Namespace name (e.g., "my.app")
    pub name: Spanned<String>,
    /// Required modules
    pub requires: Vec<RequireSpec>,
}

/// A complete liar program
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Spanned<Item>>,
}

/// Top-level items
#[derive(Debug, Clone)]
pub enum Item {
    /// Namespace declaration: (ns name (:require [...]))
    Namespace(Namespace),
    /// Function definition: (defun name (params...) body)
    Defun(Defun),
    /// Constant definition: (def name value)
    Def(Def),
    /// Struct definition: (defstruct name (fields...))
    Defstruct(Defstruct),
    /// Protocol definition: (defprotocol Name (method [self args...])...)
    Defprotocol(Defprotocol),
    /// Protocol extension: (extend-protocol ProtocolName TypeName (method [self args...] body)...)
    ExtendProtocol(ExtendProtocol),
    /// Protocol default: (extend-protocol-default TargetProtocol SourceProtocol (method [self args...] body)...)
    ExtendProtocolDefault(ExtendProtocolDefault),
    /// Macro definition: (defmacro name (params...) body)
    Defmacro(Defmacro),
    /// External function declaration: (extern name ret-type (param-types...))
    Extern(Extern),
    /// Conditional compilation: (when-target :linux items...)
    WhenTarget(WhenTarget),
}

/// Function definition
#[derive(Debug, Clone)]
pub struct Defun {
    pub name: Spanned<String>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<Type>>,
    pub body: Spanned<Expr>,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Spanned<String>,
    pub ty: Option<Spanned<Type>>,
    pub mutable: bool, // &name for mutable reference
}

/// Constant definition
#[derive(Debug, Clone)]
pub struct Def {
    pub name: Spanned<String>,
    pub value: Spanned<Expr>,
}

/// Struct definition
#[derive(Debug, Clone)]
pub struct Defstruct {
    pub name: Spanned<String>,
    pub fields: Vec<StructField>,
}

/// Struct field
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Spanned<String>,
    pub ty: Spanned<Type>,
}

/// Protocol definition (ADR-022)
#[derive(Debug, Clone)]
pub struct Defprotocol {
    pub name: Spanned<String>,
    pub doc: Option<String>,
    pub methods: Vec<ProtocolMethod>,
}

/// Protocol method signature
#[derive(Debug, Clone)]
pub struct ProtocolMethod {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>, // includes self
    pub doc: Option<String>,
}

/// Protocol extension for a specific type
#[derive(Debug, Clone)]
pub struct ExtendProtocol {
    pub protocol: Spanned<String>,
    pub type_name: Spanned<String>,
    pub implementations: Vec<MethodImpl>,
}

/// Method implementation for a type
#[derive(Debug, Clone)]
pub struct MethodImpl {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>, // includes self
    pub body: Spanned<Expr>,
}

/// Protocol default implementation for types implementing another protocol
/// (extend-protocol-default TargetProtocol SourceProtocol (method [self] body)...)
#[derive(Debug, Clone)]
pub struct ExtendProtocolDefault {
    pub protocol: Spanned<String>, // The protocol being extended (e.g., Mappable)
    pub source_protocol: Spanned<String>, // The source protocol constraint (e.g., Seq)
    pub implementations: Vec<MethodImpl>,
}

/// Macro definition
#[derive(Debug, Clone)]
pub struct Defmacro {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>,
    pub body: Spanned<Expr>,
}

/// External function declaration (FFI)
#[derive(Debug, Clone)]
pub struct Extern {
    pub name: Spanned<String>,
    pub return_type: Spanned<Type>,
    pub param_types: Vec<Spanned<Type>>,
    pub varargs: bool,
}

/// Target platform for conditional compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target {
    Linux,
    Macos,
    Windows,
    Wasi,
}

impl Target {
    /// Parse a target from a keyword string (without the colon)
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "linux" => Some(Target::Linux),
            "macos" => Some(Target::Macos),
            "windows" => Some(Target::Windows),
            "wasi" => Some(Target::Wasi),
            _ => None,
        }
    }

    /// Get the target name as a string
    pub fn as_str(&self) -> &'static str {
        match self {
            Target::Linux => "linux",
            Target::Macos => "macos",
            Target::Windows => "windows",
            Target::Wasi => "wasi",
        }
    }

    /// Detect the current host target
    pub fn host() -> Self {
        #[cfg(target_os = "linux")]
        return Target::Linux;
        #[cfg(target_os = "macos")]
        return Target::Macos;
        #[cfg(target_os = "windows")]
        return Target::Windows;
        #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
        return Target::Linux; // fallback
    }
}

/// Conditional compilation block: (when-target :linux item1 item2 ...)
#[derive(Debug, Clone)]
pub struct WhenTarget {
    pub target: Target,
    pub items: Vec<Spanned<Item>>,
}

/// Type annotation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Named type: i64, bool, String, etc.
    Named(String),
    /// Reference: &T
    Ref(Box<Type>),
    /// Mutable reference: &mut T
    RefMut(Box<Type>),
    /// Function type: (-> (A B) C)
    Fn(Vec<Type>, Box<Type>),
    /// Tuple: (A B C)
    Tuple(Vec<Type>),
    /// Unit type
    Unit,
    /// Raw pointer type (used for closure environments)
    Ptr,
    /// Closure type: { fn_ptr: ptr, env_ptr: ptr }
    /// Used for higher-order functions that capture closures
    Closure,
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    Int(i64),
    /// Float literal
    Float(f64),
    /// Boolean literal
    Bool(bool),
    /// String literal
    String(String),
    /// Nil literal
    Nil,
    /// Variable reference (possibly qualified: namespace/name)
    Var(QualifiedName),
    /// Function call: (f args...)
    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    /// Lambda: (fn (params...) body)
    Lambda(Vec<Param>, Box<Spanned<Expr>>),
    /// Let binding: (let ((name value)...) body)
    Let(Vec<LetBinding>, Box<Spanned<Expr>>),
    /// Parallel let (thread-safe): (plet ((name value)...) body)
    Plet(Vec<LetBinding>, Box<Spanned<Expr>>),
    /// If expression: (if cond then else)
    If(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// Block: (do exprs...)
    Do(Vec<Spanned<Expr>>),
    /// Set (mutation): (set! name value)
    Set(Spanned<String>, Box<Spanned<Expr>>),
    /// Reference: (ref expr)
    Ref(Box<Spanned<Expr>>),
    /// Mutable reference: (ref-mut expr)
    RefMut(Box<Spanned<Expr>>),
    /// Dereference: (deref expr)
    Deref(Box<Spanned<Expr>>),
    /// Struct literal: (Struct field: value ...)
    Struct(String, Vec<(Spanned<String>, Spanned<Expr>)>),
    /// Field access: (. expr field)
    Field(Box<Spanned<Expr>>, Spanned<String>),
    /// Quote (for atoms): 'symbol
    Quote(String),
    /// Unsafe block: (unsafe body)
    Unsafe(Box<Spanned<Expr>>),

    // Atoms for thread-safe mutable state (ADR-011)
    /// Create atom: (atom value)
    Atom(Box<Spanned<Expr>>),
    /// Atomic swap: (swap! atom fn) - applies fn to current value
    Swap(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// Atomic reset: (reset! atom value) - sets new value
    Reset(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// Atomic deref: @atom - reads current value
    AtomDeref(Box<Spanned<Expr>>),
    /// Compare and set: (compare-and-set! atom old new)
    CompareAndSet {
        atom: Box<Spanned<Expr>>,
        old: Box<Spanned<Expr>>,
        new: Box<Spanned<Expr>>,
    },

    // Persistent collections (ADR-018)
    /// Vector literal: [1 2 3]
    Vector(Vec<Spanned<Expr>>),
    /// Map literal: {:a 1 :b 2}
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    /// Keyword literal: :foo
    Keyword(String),

    // Conventional mutable collections (ADR-018)
    /// Mutable vector: <[1 2 3]>
    ConvVector(Vec<Spanned<Expr>>),
    /// Mutable map: <{:a 1 :b 2}>
    ConvMap(Vec<(Spanned<Expr>, Spanned<Expr>)>),

    // Async/await (ADR-014)
    /// Async block: (async body)
    Async(Box<Spanned<Expr>>),
    /// Await expression: (await future)
    Await(Box<Spanned<Expr>>),

    // SIMD vectors (ADR-016)
    /// SIMD vector literal: <<1 2 3 4>>
    SimdVector(Vec<Spanned<Expr>>),

    // STM (ADR-012)
    /// Dosync block: (dosync exprs...)
    Dosync(Vec<Spanned<Expr>>),
    /// Ref set in transaction: (ref-set ref value)
    RefSetStm(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// Alter in transaction: (alter ref fn args...)
    Alter {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    /// Commute in transaction: (commute ref fn args...)
    Commute {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },

    // Iterators
    /// Create iterator from collection: (iter coll)
    Iter(Box<Spanned<Expr>>),
    /// Materialize iterator to collection: (collect iter)
    Collect(Box<Spanned<Expr>>),

    // Byte arrays and regex
    /// Byte array literal: #[1 2 3] or #[0x61 0x62 0x63]
    ByteArray(Vec<u8>),
    /// Regex literal: #r"pattern"flags
    Regex { pattern: String, flags: String },

    // Overflow handling (ADR-017)
    /// Boxed arithmetic: auto-promotes to bigint on overflow
    Boxed(Box<Spanned<Expr>>),
    /// Wrapping arithmetic: C-style silent wrap
    Wrapping(Box<Spanned<Expr>>),

    // Macro syntax
    /// Quasiquote: `expr - template with possible unquotes
    Quasiquote(Box<Spanned<Expr>>),
    /// Unquote: ,expr - insert value into quasiquote
    Unquote(Box<Spanned<Expr>>),
    /// Unquote-splicing: ,@expr - splice list into quasiquote
    UnquoteSplicing(Box<Spanned<Expr>>),
    /// Generate unique symbol: (gensym) or (gensym "prefix")
    Gensym(Option<String>),

    // Closure conversion (generated by closure pass, not parsed)
    /// Closure struct literal: { fn_ptr, env_ptr }
    /// fn_name is the name of the lifted lambda function
    /// env is the heap-allocated environment, or None for null
    ClosureLit {
        fn_name: String,
        env: Option<Box<Spanned<Expr>>>,
    },
    /// Heap-allocated closure environment
    /// Generated by closure conversion for escaping closures
    HeapEnvAlloc {
        struct_name: String,
        fields: Vec<(Spanned<String>, Spanned<Expr>)>,
    },
    /// Stack-allocated closure environment
    /// Generated by closure conversion for non-escaping closures
    StackEnvAlloc {
        struct_name: String,
        fields: Vec<(Spanned<String>, Spanned<Expr>)>,
    },
}

/// Let binding
#[derive(Debug, Clone)]
pub struct LetBinding {
    pub name: Spanned<String>,
    pub ty: Option<Spanned<Type>>,
    pub value: Spanned<Expr>,
}
