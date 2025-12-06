//! AST types for lIR expressions

/// Scalar types in lIR
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    I1,
    I8,
    I16,
    I32,
    I64,
    Float,
    Double,
    Void, // For void return type
}

impl ScalarType {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::I1 | Self::I8 | Self::I16 | Self::I32 | Self::I64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float | Self::Double)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            Self::I1 => 1,
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
            Self::Float => 32,
            Self::Double => 64,
            Self::Void => 0,
        }
    }
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I1 => write!(f, "i1"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
            Self::Void => write!(f, "void"),
        }
    }
}

/// Vector type: <N x scalar_type>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VectorType {
    pub count: u32,
    pub element: ScalarType,
}

impl std::fmt::Display for VectorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} x {}>", self.count, self.element)
    }
}

/// Type in lIR (scalar or vector or pointer)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Scalar(ScalarType),
    Vector(VectorType),
    Ptr, // Opaque pointer type (modern LLVM)
}

impl Type {
    pub fn is_integer(&self) -> bool {
        match self {
            Self::Scalar(s) => s.is_integer(),
            Self::Vector(v) => v.element.is_integer(),
            Self::Ptr => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Scalar(s) => s.is_float(),
            Self::Vector(v) => v.element.is_float(),
            Self::Ptr => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Ptr)
    }

    pub fn is_i1(&self) -> bool {
        matches!(self, Self::Scalar(ScalarType::I1))
    }

    pub fn scalar(&self) -> Option<&ScalarType> {
        match self {
            Self::Scalar(s) => Some(s),
            Self::Vector(_) | Self::Ptr => None,
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            Self::Scalar(s) => s.bit_width(),
            Self::Vector(v) => v.element.bit_width(),
            Self::Ptr => 64, // Assume 64-bit pointers
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Vector(v) => write!(f, "{}", v),
            Self::Ptr => write!(f, "ptr"),
        }
    }
}

impl From<ScalarType> for Type {
    fn from(s: ScalarType) -> Self {
        Self::Scalar(s)
    }
}

impl From<VectorType> for Type {
    fn from(v: VectorType) -> Self {
        Self::Vector(v)
    }
}

/// Integer comparison predicates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ICmpPred {
    Eq,
    Ne,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl std::fmt::Display for ICmpPred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "eq"),
            Self::Ne => write!(f, "ne"),
            Self::Slt => write!(f, "slt"),
            Self::Sle => write!(f, "sle"),
            Self::Sgt => write!(f, "sgt"),
            Self::Sge => write!(f, "sge"),
            Self::Ult => write!(f, "ult"),
            Self::Ule => write!(f, "ule"),
            Self::Ugt => write!(f, "ugt"),
            Self::Uge => write!(f, "uge"),
        }
    }
}

/// Memory ordering for atomic operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryOrdering {
    Monotonic, // Minimal ordering, no synchronization
    Acquire,   // Acquire semantics (for loads)
    Release,   // Release semantics (for stores)
    AcqRel,    // Acquire-release (for read-modify-write)
    SeqCst,    // Sequential consistency (strongest)
}

impl std::fmt::Display for MemoryOrdering {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Monotonic => write!(f, "monotonic"),
            Self::Acquire => write!(f, "acquire"),
            Self::Release => write!(f, "release"),
            Self::AcqRel => write!(f, "acq_rel"),
            Self::SeqCst => write!(f, "seq_cst"),
        }
    }
}

/// Atomic read-modify-write operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtomicRMWOp {
    Xchg, // Exchange (swap)
    Add,  // Add
    Sub,  // Subtract
    And,  // Bitwise AND
    Or,   // Bitwise OR
    Xor,  // Bitwise XOR
    Min,  // Signed minimum
    Max,  // Signed maximum
    UMin, // Unsigned minimum
    UMax, // Unsigned maximum
}

impl std::fmt::Display for AtomicRMWOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Xchg => write!(f, "xchg"),
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Min => write!(f, "min"),
            Self::Max => write!(f, "max"),
            Self::UMin => write!(f, "umin"),
            Self::UMax => write!(f, "umax"),
        }
    }
}

/// Float comparison predicates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FCmpPred {
    // Ordered (false if NaN)
    Oeq,
    One,
    Olt,
    Ole,
    Ogt,
    Oge,
    Ord,
    // Unordered (true if NaN)
    Ueq,
    Une,
    Ult,
    Ule,
    Ugt,
    Uge,
    Uno,
}

impl std::fmt::Display for FCmpPred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Oeq => write!(f, "oeq"),
            Self::One => write!(f, "one"),
            Self::Olt => write!(f, "olt"),
            Self::Ole => write!(f, "ole"),
            Self::Ogt => write!(f, "ogt"),
            Self::Oge => write!(f, "oge"),
            Self::Ord => write!(f, "ord"),
            Self::Ueq => write!(f, "ueq"),
            Self::Une => write!(f, "une"),
            Self::Ult => write!(f, "ult"),
            Self::Ule => write!(f, "ule"),
            Self::Ugt => write!(f, "ugt"),
            Self::Uge => write!(f, "uge"),
            Self::Uno => write!(f, "uno"),
        }
    }
}

/// Special float values
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatValue {
    Number(f64),
    Inf,
    NegInf,
    Nan,
}

/// Expression AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    IntLit {
        ty: ScalarType,
        value: i128,
    },
    FloatLit {
        ty: ScalarType,
        value: FloatValue,
    },
    VectorLit {
        ty: VectorType,
        elements: Vec<Expr>,
    },
    // Null pointer literal
    NullPtr,

    // String literal (null-terminated [N x i8] array)
    StringLit(String),

    // Struct literal: { val1, val2, ... }
    StructLit(Vec<Expr>),

    // Aggregate operations
    ExtractValue {
        aggregate: Box<Expr>,
        indices: Vec<u32>,
    },
    InsertValue {
        aggregate: Box<Expr>,
        value: Box<Expr>,
        indices: Vec<u32>,
    },

    // Integer arithmetic
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    SDiv(Box<Expr>, Box<Expr>),
    UDiv(Box<Expr>, Box<Expr>),
    SRem(Box<Expr>, Box<Expr>),
    URem(Box<Expr>, Box<Expr>),

    // Float arithmetic
    FAdd(Box<Expr>, Box<Expr>),
    FSub(Box<Expr>, Box<Expr>),
    FMul(Box<Expr>, Box<Expr>),
    FDiv(Box<Expr>, Box<Expr>),
    FRem(Box<Expr>, Box<Expr>),

    // Bitwise
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Shl(Box<Expr>, Box<Expr>),
    LShr(Box<Expr>, Box<Expr>),
    AShr(Box<Expr>, Box<Expr>),

    // Comparisons
    ICmp {
        pred: ICmpPred,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FCmp {
        pred: FCmpPred,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    // Conversions
    Trunc {
        ty: ScalarType,
        value: Box<Expr>,
    },
    ZExt {
        ty: ScalarType,
        value: Box<Expr>,
    },
    SExt {
        ty: ScalarType,
        value: Box<Expr>,
    },
    FPTrunc {
        ty: ScalarType,
        value: Box<Expr>,
    },
    FPExt {
        ty: ScalarType,
        value: Box<Expr>,
    },
    FPToUI {
        ty: ScalarType,
        value: Box<Expr>,
    },
    FPToSI {
        ty: ScalarType,
        value: Box<Expr>,
    },
    UIToFP {
        ty: ScalarType,
        value: Box<Expr>,
    },
    SIToFP {
        ty: ScalarType,
        value: Box<Expr>,
    },

    // Control flow
    Select {
        cond: Box<Expr>,
        true_val: Box<Expr>,
        false_val: Box<Expr>,
    },

    // Vector operations
    ExtractElement {
        vec: Box<Expr>,
        idx: Box<Expr>,
    },
    InsertElement {
        vec: Box<Expr>,
        val: Box<Expr>,
        idx: Box<Expr>,
    },
    ShuffleVector {
        vec1: Box<Expr>,
        vec2: Box<Expr>,
        mask: Box<Expr>,
    },

    // Local variable reference
    LocalRef(String),

    // Global/function reference (for function pointers)
    GlobalRef(String),

    // Return instruction
    Ret(Option<Box<Expr>>),

    // Memory operations
    Alloca {
        ty: ParamType,
        count: Option<Box<Expr>>,
    },
    Load {
        ty: ParamType,
        ptr: Box<Expr>,
    },
    Store {
        value: Box<Expr>,
        ptr: Box<Expr>,
    },

    // Ownership operations (verified by separate pass, compile to no-ops or simple ops)
    // (alloc own T) - allocate owned value
    AllocOwn {
        elem_type: ScalarType,
    },
    // (borrow ref x) - create shared borrow
    BorrowRef {
        value: Box<Expr>,
    },
    // (borrow refmut x) - create mutable borrow
    BorrowRefMut {
        value: Box<Expr>,
    },
    // (drop x) - explicit drop
    Drop {
        value: Box<Expr>,
    },
    // (move x) - explicit move
    Move {
        value: Box<Expr>,
    },

    // Pointer arithmetic
    GetElementPtr {
        ty: GepType,        // The element type we're indexing through (scalar or struct)
        ptr: Box<Expr>,     // The base pointer
        indices: Vec<Expr>, // One or more indices
        inbounds: bool,     // Whether to use inbounds flag
    },

    // Control flow
    Br(BranchTarget),
    Phi {
        ty: ScalarType,
        incoming: Vec<(String, Box<Expr>)>, // (block_label, value)
    },

    // Function call
    Call {
        name: String,
        args: Vec<Expr>,
    },

    // Tail call (guaranteed tail call optimization)
    TailCall {
        name: String,
        args: Vec<Expr>,
    },

    // Indirect call through function pointer
    IndirectCall {
        fn_ptr: Box<Expr>,
        ret_ty: ParamType,
        args: Vec<Expr>,
    },

    // Array operations (bounds-checked)
    // (array-alloc type size) - allocate array on stack
    ArrayAlloc {
        elem_type: ScalarType,
        size: u32,
    },
    // (array-get type size arr idx) - bounds-checked read
    ArrayGet {
        elem_type: ScalarType,
        size: u32,
        array: Box<Expr>,
        index: Box<Expr>,
    },
    // (array-set type size arr idx val) - bounds-checked write
    ArraySet {
        elem_type: ScalarType,
        size: u32,
        array: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    // (array-len size) - returns compile-time constant size
    ArrayLen {
        size: u32,
    },
    // (array-ptr arr) - get raw pointer (for FFI)
    ArrayPtr {
        array: Box<Expr>,
    },

    // Reference counting operations
    // (rc-alloc T) - allocate with refcount 1, returns rc T
    RcAlloc {
        elem_type: ScalarType,
    },
    // (rc-clone x) - increment refcount, return alias
    RcClone {
        value: Box<Expr>,
    },
    // (rc-drop x) - decrement refcount, free if zero
    RcDrop {
        value: Box<Expr>,
    },
    // (rc-count x) - get current refcount (for debugging)
    RcCount {
        value: Box<Expr>,
    },
    // (rc-ptr x) - get raw pointer (for load/store)
    RcPtr {
        value: Box<Expr>,
    },

    // Atomic memory operations
    // (atomic-load ordering type ptr) - atomic load
    AtomicLoad {
        ordering: MemoryOrdering,
        ty: ScalarType,
        ptr: Box<Expr>,
    },
    // (atomic-store ordering value ptr) - atomic store
    AtomicStore {
        ordering: MemoryOrdering,
        value: Box<Expr>,
        ptr: Box<Expr>,
    },
    // (atomicrmw op ordering ptr value) - atomic read-modify-write
    AtomicRMW {
        op: AtomicRMWOp,
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        value: Box<Expr>,
    },
    // (cmpxchg ordering ptr expected new) - compare-and-exchange
    // Returns { old_value, success_flag } struct
    CmpXchg {
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        expected: Box<Expr>,
        new_value: Box<Expr>,
    },

    // (fence ordering) - memory barrier
    Fence {
        ordering: MemoryOrdering,
    },

    // Let binding for SSA values
    Let {
        bindings: Vec<(String, Box<Expr>)>, // (name, value) pairs
        body: Vec<Expr>,                    // body expressions, returns last
    },
}

/// Branch target - either unconditional or conditional
#[derive(Debug, Clone, PartialEq)]
pub enum BranchTarget {
    Unconditional(String),
    Conditional {
        cond: Box<Expr>,
        true_label: String,
        false_label: String,
    },
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ty: ParamType,
    pub name: String,
}

/// A basic block with a label and instructions
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Expr>,
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub return_type: ReturnType,
    pub params: Vec<Param>,
    pub blocks: Vec<BasicBlock>,
}

/// Parameter type in external declarations (simpler than Param - no name)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamType {
    Scalar(ScalarType),
    Ptr,
    // Ownership types (compile to ptr at LLVM level, verified statically)
    Own(Box<ScalarType>),    // Owned pointer - dropped when out of scope
    Ref(Box<ScalarType>),    // Shared borrow - read-only, lifetime-bound
    RefMut(Box<ScalarType>), // Mutable borrow - exclusive, lifetime-bound
    // Reference-counted pointer
    Rc(Box<ScalarType>), // Reference-counted pointer with atomic inc/dec
    // Anonymous struct (for closure parameters)
    AnonStruct(Vec<ParamType>),
}

impl std::fmt::Display for ParamType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Ptr => write!(f, "ptr"),
            Self::Own(t) => write!(f, "own {}", t),
            Self::Ref(t) => write!(f, "ref {}", t),
            Self::RefMut(t) => write!(f, "refmut {}", t),
            Self::Rc(t) => write!(f, "rc {}", t),
            Self::AnonStruct(fields) => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, " }}")
            }
        }
    }
}

/// Type for getelementptr - can be scalar, ptr, or named struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GepType {
    Scalar(ScalarType),
    Ptr,
    Struct(String), // Named struct (e.g., "point" for %struct.point)
}

impl std::fmt::Display for GepType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Ptr => write!(f, "ptr"),
            Self::Struct(name) => write!(f, "%struct.{}", name),
        }
    }
}

/// Return type for functions (scalar, ptr, or anonymous struct)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnType {
    Scalar(ScalarType),
    Ptr,
    /// Anonymous struct with given field types (for closure returns)
    AnonStruct(Vec<ParamType>),
}

impl std::fmt::Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Ptr => write!(f, "ptr"),
            Self::AnonStruct(fields) => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, " }}")
            }
        }
    }
}

/// External function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ExternDecl {
    pub name: String,
    pub return_type: ReturnType,
    pub param_types: Vec<ParamType>,
    pub varargs: bool,
}

/// Global variable definition
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalDef {
    pub name: String,
    pub ty: ParamType,     // Type of the value (scalar or ptr)
    pub initializer: Expr, // Initial value
    pub is_constant: bool, // true for constant (immutable), false for global (mutable)
}

/// Struct type definition
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<ParamType>, // Field types (can be scalar or ptr)
}

/// Top-level module item
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(FunctionDef),
    ExternDecl(ExternDecl),
    Global(GlobalDef),
    Struct(StructDef),
}
