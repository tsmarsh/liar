use thiserror::Error;

#[derive(Error, Debug)]
pub enum LirError {
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("{0}")]
    Type(#[from] TypeError),
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token: {0}")]
    UnexpectedToken(String),

    #[error("expected {expected}, found {found}")]
    Expected { expected: String, found: String },

    #[error("invalid number: {0}")]
    InvalidNumber(String),

    #[error("unknown operation: {0}")]
    UnknownOperation(String),

    #[error("unknown type: {0}")]
    UnknownType(String),

    #[error("unknown predicate: {0}")]
    UnknownPredicate(String),
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("type mismatch")]
    TypeMismatch,

    #[error("integer operation on float")]
    IntOpOnFloat,

    #[error("float operation on integer")]
    FloatOpOnInt,

    #[error("bitwise operation on float")]
    BitwiseOpOnFloat,

    #[error("icmp requires integer")]
    IcmpRequiresInt,

    #[error("fcmp requires float")]
    FcmpRequiresFloat,

    #[error("condition must be i1")]
    ConditionMustBeI1,

    #[error("cannot truncate to larger type")]
    CannotTruncToLarger,

    #[error("cannot extend to smaller type")]
    CannotExtendToSmaller,

    #[error("fptosi requires float source")]
    FptosiRequiresFloat,

    #[error("fptoui requires float source")]
    FptouiRequiresFloat,

    #[error("sitofp requires integer source")]
    SitofpRequiresInt,

    #[error("uitofp requires integer source")]
    UitofpRequiresInt,

    #[error("fptrunc requires float types")]
    FptruncRequiresFloat,

    #[error("fpext requires float types")]
    FpextRequiresFloat,

    #[error("vector element count mismatch")]
    VectorCountMismatch,

    #[error("vector index must be i32")]
    VectorIndexMustBeI32,
}

pub type Result<T> = std::result::Result<T, LirError>;
