//! LLVM IR code generation

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::types::{BasicTypeEnum, IntType};
use inkwell::values::BasicValueEnum;

use lir_core::ast::{Expr, FloatValue, ICmpPred, FCmpPred, ScalarType, Type};
use lir_core::types::TypeChecker;
use lir_core::error::TypeError;
use inkwell::{IntPredicate, FloatPredicate};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error("type error: {0}")]
    Type(#[from] TypeError),
    #[error("codegen error: {0}")]
    CodeGen(String),
    #[error("not yet implemented: {0}")]
    NotImplemented(String),
}

pub type Result<T> = std::result::Result<T, CodeGenError>;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
        }
    }

    /// Get LLVM integer type from ScalarType
    fn int_type(&self, ty: &ScalarType) -> IntType<'ctx> {
        match ty {
            ScalarType::I1 => self.context.bool_type(),
            ScalarType::I8 => self.context.i8_type(),
            ScalarType::I16 => self.context.i16_type(),
            ScalarType::I32 => self.context.i32_type(),
            ScalarType::I64 => self.context.i64_type(),
            _ => panic!("not an integer type: {}", ty),
        }
    }

    /// Get LLVM type from lIR Type
    #[allow(dead_code)]
    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Scalar(s) => {
                if s.is_integer() {
                    self.int_type(s).into()
                } else {
                    match s {
                        ScalarType::Float => self.context.f32_type().into(),
                        ScalarType::Double => self.context.f64_type().into(),
                        _ => unreachable!(),
                    }
                }
            }
            Type::Vector(v) => {
                if v.element.is_integer() {
                    self.int_type(&v.element).vec_type(v.count).into()
                } else {
                    match v.element {
                        ScalarType::Float => self.context.f32_type().vec_type(v.count).into(),
                        ScalarType::Double => self.context.f64_type().vec_type(v.count).into(),
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    /// Compile expression to LLVM value
    pub fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        // Type check first
        let checker = TypeChecker::new();
        let _ty = checker.check(expr)?;

        match expr {
            // Integer literal
            Expr::IntLit { ty: scalar_ty, value } => {
                let llvm_ty = self.int_type(scalar_ty);
                // Convert i128 to u64 for LLVM (handles sign correctly)
                let val = *value as u64;
                Ok(llvm_ty.const_int(val, scalar_ty != &ScalarType::I1 && *value < 0).into())
            }

            // Integer arithmetic
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_add(lhs_val, rhs_val, "add")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_sub(lhs_val, rhs_val, "sub")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_mul(lhs_val, rhs_val, "mul")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::SDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_signed_div(lhs_val, rhs_val, "sdiv")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::UDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_unsigned_div(lhs_val, rhs_val, "udiv")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::SRem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_signed_rem(lhs_val, rhs_val, "srem")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::URem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_int_unsigned_rem(lhs_val, rhs_val, "urem")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Float literal
            Expr::FloatLit { ty: scalar_ty, value } => {
                let fval = match value {
                    FloatValue::Number(n) => *n,
                    FloatValue::Inf => f64::INFINITY,
                    FloatValue::NegInf => f64::NEG_INFINITY,
                    FloatValue::Nan => f64::NAN,
                };
                match scalar_ty {
                    ScalarType::Float => {
                        Ok(self.context.f32_type().const_float(fval).into())
                    }
                    ScalarType::Double => {
                        Ok(self.context.f64_type().const_float(fval).into())
                    }
                    _ => Err(CodeGenError::CodeGen("invalid float type".to_string())),
                }
            }

            // Float arithmetic
            Expr::FAdd(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                Ok(self.builder.build_float_add(lhs_val, rhs_val, "fadd")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FSub(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                Ok(self.builder.build_float_sub(lhs_val, rhs_val, "fsub")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FMul(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                Ok(self.builder.build_float_mul(lhs_val, rhs_val, "fmul")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                Ok(self.builder.build_float_div(lhs_val, rhs_val, "fdiv")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FRem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                Ok(self.builder.build_float_rem(lhs_val, rhs_val, "frem")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Bitwise operations
            Expr::And(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_and(lhs_val, rhs_val, "and")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::Or(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_or(lhs_val, rhs_val, "or")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::Xor(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_xor(lhs_val, rhs_val, "xor")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::Shl(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_left_shift(lhs_val, rhs_val, "shl")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::LShr(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_right_shift(lhs_val, rhs_val, false, "lshr")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::AShr(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self.builder.build_right_shift(lhs_val, rhs_val, true, "ashr")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Integer comparison
            Expr::ICmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                let llvm_pred = match pred {
                    ICmpPred::Eq => IntPredicate::EQ,
                    ICmpPred::Ne => IntPredicate::NE,
                    ICmpPred::Slt => IntPredicate::SLT,
                    ICmpPred::Sle => IntPredicate::SLE,
                    ICmpPred::Sgt => IntPredicate::SGT,
                    ICmpPred::Sge => IntPredicate::SGE,
                    ICmpPred::Ult => IntPredicate::ULT,
                    ICmpPred::Ule => IntPredicate::ULE,
                    ICmpPred::Ugt => IntPredicate::UGT,
                    ICmpPred::Uge => IntPredicate::UGE,
                };
                Ok(self.builder.build_int_compare(llvm_pred, lhs_val, rhs_val, "icmp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Float comparison
            Expr::FCmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr(lhs)?.into_float_value();
                let rhs_val = self.compile_expr(rhs)?.into_float_value();
                let llvm_pred = match pred {
                    FCmpPred::Oeq => FloatPredicate::OEQ,
                    FCmpPred::One => FloatPredicate::ONE,
                    FCmpPred::Olt => FloatPredicate::OLT,
                    FCmpPred::Ole => FloatPredicate::OLE,
                    FCmpPred::Ogt => FloatPredicate::OGT,
                    FCmpPred::Oge => FloatPredicate::OGE,
                    FCmpPred::Ord => FloatPredicate::ORD,
                    FCmpPred::Ueq => FloatPredicate::UEQ,
                    FCmpPred::Une => FloatPredicate::UNE,
                    FCmpPred::Ult => FloatPredicate::ULT,
                    FCmpPred::Ule => FloatPredicate::ULE,
                    FCmpPred::Ugt => FloatPredicate::UGT,
                    FCmpPred::Uge => FloatPredicate::UGE,
                    FCmpPred::Uno => FloatPredicate::UNO,
                };
                Ok(self.builder.build_float_compare(llvm_pred, lhs_val, rhs_val, "fcmp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Integer conversions
            Expr::Trunc { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self.builder.build_int_truncate(val, target, "trunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::ZExt { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self.builder.build_int_z_extend(val, target, "zext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::SExt { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self.builder.build_int_s_extend(val, target, "sext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Float conversions
            Expr::FPTrunc { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fptrunc target".to_string())),
                };
                Ok(self.builder.build_float_trunc(val, target, "fptrunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FPExt { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fpext target".to_string())),
                };
                Ok(self.builder.build_float_ext(val, target, "fpext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Float <-> Int conversions
            Expr::FPToUI { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = self.int_type(target_ty);
                Ok(self.builder.build_float_to_unsigned_int(val, target, "fptoui")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::FPToSI { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = self.int_type(target_ty);
                Ok(self.builder.build_float_to_signed_int(val, target, "fptosi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::UIToFP { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid uitofp target".to_string())),
                };
                Ok(self.builder.build_unsigned_int_to_float(val, target, "uitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }
            Expr::SIToFP { ty: target_ty, value } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid sitofp target".to_string())),
                };
                Ok(self.builder.build_signed_int_to_float(val, target, "sitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?.into())
            }

            // Not yet implemented
            _ => Err(CodeGenError::NotImplemented(format!("expression type not yet supported"))),
        }
    }

    /// Create a JIT function that evaluates the expression and returns the result
    pub fn create_eval_function(&self, expr: &Expr) -> Result<()> {
        // Type check to get result type
        let checker = TypeChecker::new();
        let ty = checker.check(expr)?;

        let fn_type = match &ty {
            Type::Scalar(s) if s.is_integer() => {
                self.int_type(s).fn_type(&[], false)
            }
            Type::Scalar(ScalarType::Float) => {
                self.context.f32_type().fn_type(&[], false)
            }
            Type::Scalar(ScalarType::Double) => {
                self.context.f64_type().fn_type(&[], false)
            }
            _ => return Err(CodeGenError::NotImplemented("non-scalar return types".to_string())),
        };

        let function = self.module.add_function("__lir_eval", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let result = self.compile_expr(expr)?;
        self.builder.build_return(Some(&result))
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(())
    }
}

/// Typed result from JIT evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I1(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Float(f32),
    Double(f64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I1(v) => write!(f, "(i1 {})", if *v { 1 } else { 0 }),
            Value::I8(v) => write!(f, "(i8 {})", v),
            Value::I16(v) => write!(f, "(i16 {})", v),
            Value::I32(v) => write!(f, "(i32 {})", v),
            Value::I64(v) => write!(f, "(i64 {})", v),
            Value::Float(v) => {
                if v.is_nan() {
                    write!(f, "(float nan)")
                } else if v.is_infinite() {
                    if *v > 0.0 {
                        write!(f, "(float inf)")
                    } else {
                        write!(f, "(float -inf)")
                    }
                } else {
                    write!(f, "(float {})", v)
                }
            }
            Value::Double(v) => {
                if v.is_nan() {
                    write!(f, "(double nan)")
                } else if v.is_infinite() {
                    if *v > 0.0 {
                        write!(f, "(double inf)")
                    } else {
                        write!(f, "(double -inf)")
                    }
                } else {
                    write!(f, "(double {})", v)
                }
            }
        }
    }
}
