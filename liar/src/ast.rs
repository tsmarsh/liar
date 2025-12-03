//! liar Abstract Syntax Tree

use crate::span::Spanned;

/// A complete liar program
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Spanned<Item>>,
}

/// Top-level items
#[derive(Debug, Clone)]
pub enum Item {
    /// Function definition: (defun name (params...) body)
    Defun(Defun),
    /// Constant definition: (def name value)
    Def(Def),
    /// Struct definition: (defstruct name (fields...))
    Defstruct(Defstruct),
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
    /// Variable reference
    Var(String),
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
    /// Match expression: (match expr (pattern body)...)
    Match(Box<Spanned<Expr>>, Vec<MatchArm>),
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
}

/// Let binding
#[derive(Debug, Clone)]
pub struct LetBinding {
    pub name: Spanned<String>,
    pub ty: Option<Spanned<Type>>,
    pub value: Spanned<Expr>,
}

/// Match arm
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expr>,
}

/// Pattern for match
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Wildcard: _
    Wildcard,
    /// Variable binding
    Var(String),
    /// Literal
    Literal(Literal),
    /// Struct destructure: (Struct field: var ...)
    Struct(String, Vec<(String, Pattern)>),
    /// Tuple destructure: (a b c)
    Tuple(Vec<Pattern>),
}

/// Literal values (subset of Expr for patterns)
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
}
