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
}

impl std::fmt::Display for ParamType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Ptr => write!(f, "ptr"),
        }
    }
}

/// Type for getelementptr - can be scalar or named struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GepType {
    Scalar(ScalarType),
    Struct(String), // Named struct (e.g., "point" for %struct.point)
}

impl std::fmt::Display for GepType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Struct(name) => write!(f, "%struct.{}", name),
        }
    }
}

/// Return type for functions (scalar or ptr)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnType {
    Scalar(ScalarType),
    Ptr,
}

impl std::fmt::Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Ptr => write!(f, "ptr"),
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
