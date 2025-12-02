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

    pub fn bit_width(&self) -> u32 {
        match self {
            Self::I1 => 1,
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
            Self::Float => 32,
            Self::Double => 64,
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

/// Type in lIR (scalar or vector)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Scalar(ScalarType),
    Vector(VectorType),
}

impl Type {
    pub fn is_integer(&self) -> bool {
        match self {
            Self::Scalar(s) => s.is_integer(),
            Self::Vector(v) => v.element.is_integer(),
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Scalar(s) => s.is_float(),
            Self::Vector(v) => v.element.is_float(),
        }
    }

    pub fn is_i1(&self) -> bool {
        matches!(self, Self::Scalar(ScalarType::I1))
    }

    pub fn scalar(&self) -> Option<&ScalarType> {
        match self {
            Self::Scalar(s) => Some(s),
            Self::Vector(_) => None,
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            Self::Scalar(s) => s.bit_width(),
            Self::Vector(v) => v.element.bit_width(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Vector(v) => write!(f, "{}", v),
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
}
