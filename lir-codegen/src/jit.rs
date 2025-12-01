//! JIT execution engine

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

use lir_core::ast::{Expr, ScalarType, Type};
use lir_core::types::TypeChecker;

use crate::codegen::{CodeGen, CodeGenError, Value};

pub type Result<T> = std::result::Result<T, CodeGenError>;

// JIT function type aliases
type I1Func = unsafe extern "C" fn() -> bool;
type I8Func = unsafe extern "C" fn() -> i8;
type I16Func = unsafe extern "C" fn() -> i16;
type I32Func = unsafe extern "C" fn() -> i32;
type I64Func = unsafe extern "C" fn() -> i64;
type FloatFunc = unsafe extern "C" fn() -> f32;
type DoubleFunc = unsafe extern "C" fn() -> f64;

pub struct JitEngine<'ctx> {
    pub context: &'ctx Context,
}

impl<'ctx> JitEngine<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self { context }
    }

    /// Evaluate an expression using JIT compilation
    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        // Type check to get result type
        let checker = TypeChecker::new();
        let ty = checker.check(expr)?;

        // Create codegen and compile
        let codegen = CodeGen::new(self.context, "lir_eval");
        codegen.create_eval_function(expr)?;

        // Create execution engine
        let execution_engine = codegen.module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Execute based on return type
        match ty {
            Type::Scalar(ScalarType::I1) => {
                let func: JitFunction<I1Func> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I1(result))
            }
            Type::Scalar(ScalarType::I8) => {
                let func: JitFunction<I8Func> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I8(result))
            }
            Type::Scalar(ScalarType::I16) => {
                let func: JitFunction<I16Func> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I16(result))
            }
            Type::Scalar(ScalarType::I32) => {
                let func: JitFunction<I32Func> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I32(result))
            }
            Type::Scalar(ScalarType::I64) => {
                let func: JitFunction<I64Func> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I64(result))
            }
            Type::Scalar(ScalarType::Float) => {
                let func: JitFunction<FloatFunc> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Float(result))
            }
            Type::Scalar(ScalarType::Double) => {
                let func: JitFunction<DoubleFunc> = unsafe {
                    execution_engine.get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Double(result))
            }
            Type::Vector(_) => {
                Err(CodeGenError::NotImplemented("vector return types".to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lir_core::ast::{FloatValue, ScalarType};

    #[test]
    fn test_integer_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::IntLit { ty: ScalarType::I32, value: 42 };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_add() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Add(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 5 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 7 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(12));
    }

    #[test]
    fn test_sub() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Sub(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 10 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 3 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(7));
    }

    #[test]
    fn test_mul() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Mul(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 6 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 7 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_sdiv() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Signed division: -10 / 3 = -3
        let expr = Expr::SDiv(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: -10 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 3 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-3));
    }

    #[test]
    fn test_udiv() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::UDiv(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 10 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 3 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(3));
    }

    #[test]
    fn test_srem() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Signed remainder: -10 % 3 = -1
        let expr = Expr::SRem(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: -10 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 3 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-1));
    }

    #[test]
    fn test_urem() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::URem(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 10 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 3 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(1));
    }

    #[test]
    fn test_i8_overflow() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 127 + 1 wraps to -128 for i8
        let expr = Expr::Add(
            Box::new(Expr::IntLit { ty: ScalarType::I8, value: 127 }),
            Box::new(Expr::IntLit { ty: ScalarType::I8, value: 1 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I8(-128));
    }

    #[test]
    fn test_i64() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Add(
            Box::new(Expr::IntLit { ty: ScalarType::I64, value: 1_000_000_000_000 }),
            Box::new(Expr::IntLit { ty: ScalarType::I64, value: 1 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I64(1_000_000_000_001));
    }

    #[test]
    fn test_i1() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::IntLit { ty: ScalarType::I1, value: 1 };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    // Float tests

    #[test]
    fn test_double_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(3.14) };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(3.14));
    }

    #[test]
    fn test_float_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit { ty: ScalarType::Float, value: FloatValue::Number(2.5) };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Float(2.5));
    }

    #[test]
    fn test_fadd() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FAdd(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(5.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(6.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(11.0));
    }

    #[test]
    fn test_fsub() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FSub(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(10.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(3.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(7.0));
    }

    #[test]
    fn test_fmul() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FMul(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(6.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(7.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(42.0));
    }

    #[test]
    fn test_fdiv() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FDiv(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(10.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(4.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(2.5));
    }

    #[test]
    fn test_frem() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FRem(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(10.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(3.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(1.0));
    }

    #[test]
    fn test_float_inf() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Inf };
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Double(v) => assert!(v.is_infinite() && v.is_sign_positive()),
            _ => panic!("expected Double"),
        }
    }

    #[test]
    fn test_float_neg_inf() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::NegInf };
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Double(v) => assert!(v.is_infinite() && v.is_sign_negative()),
            _ => panic!("expected Double"),
        }
    }

    #[test]
    fn test_float_nan() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Nan };
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Double(v) => assert!(v.is_nan()),
            _ => panic!("expected Double"),
        }
    }

    #[test]
    fn test_fdiv_by_zero() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Division by zero produces infinity in IEEE 754
        let expr = Expr::FDiv(
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(1.0) }),
            Box::new(Expr::FloatLit { ty: ScalarType::Double, value: FloatValue::Number(0.0) }),
        );
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Double(v) => assert!(v.is_infinite()),
            _ => panic!("expected Double"),
        }
    }

    // Bitwise tests

    #[test]
    fn test_and() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 0b1100 & 0b1010 = 0b1000 = 8
        let expr = Expr::And(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1100 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1010 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(0b1000));
    }

    #[test]
    fn test_or() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 0b1100 | 0b1010 = 0b1110 = 14
        let expr = Expr::Or(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1100 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1010 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(0b1110));
    }

    #[test]
    fn test_xor() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 0b1100 ^ 0b1010 = 0b0110 = 6
        let expr = Expr::Xor(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1100 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 0b1010 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(0b0110));
    }

    #[test]
    fn test_shl() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 1 << 4 = 16
        let expr = Expr::Shl(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 1 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 4 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(16));
    }

    #[test]
    fn test_lshr() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 16 >> 2 (logical) = 4
        let expr = Expr::LShr(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 16 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 2 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(4));
    }

    #[test]
    fn test_ashr_positive() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // 16 >> 2 (arithmetic, positive) = 4
        let expr = Expr::AShr(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 16 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 2 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(4));
    }

    #[test]
    fn test_ashr_negative() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // -8 >> 2 (arithmetic) = -2 (sign-extended)
        let expr = Expr::AShr(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: -8 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 2 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-2));
    }

    #[test]
    fn test_lshr_negative() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // -1 >> 1 (logical) = large positive (zero-extended)
        // -1 in i32 is 0xFFFFFFFF, logical right shift by 1 = 0x7FFFFFFF = 2147483647
        let expr = Expr::LShr(
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: -1 }),
            Box::new(Expr::IntLit { ty: ScalarType::I32, value: 1 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(0x7FFFFFFF));
    }

    #[test]
    fn test_i1_and() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i1: 1 & 0 = 0
        let expr = Expr::And(
            Box::new(Expr::IntLit { ty: ScalarType::I1, value: 1 }),
            Box::new(Expr::IntLit { ty: ScalarType::I1, value: 0 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(false));
    }

    #[test]
    fn test_i1_or() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i1: 1 | 0 = 1
        let expr = Expr::Or(
            Box::new(Expr::IntLit { ty: ScalarType::I1, value: 1 }),
            Box::new(Expr::IntLit { ty: ScalarType::I1, value: 0 }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }
}
