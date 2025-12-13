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
