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
    use lir_core::ast::ScalarType;

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
}
