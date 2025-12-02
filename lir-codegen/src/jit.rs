//! JIT execution engine

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

#[cfg(test)]
use lir_core::ast::ParamType;
use lir_core::ast::{Expr, FunctionDef, ReturnType, ScalarType, Type, VectorType};
use lir_core::types::TypeChecker;

use crate::codegen::{CodeGen, CodeGenError, Value};

pub type Result<T> = std::result::Result<T, CodeGenError>;

// JIT function type aliases for scalar returns
type I1Func = unsafe extern "C" fn() -> bool;
type I8Func = unsafe extern "C" fn() -> i8;
type I16Func = unsafe extern "C" fn() -> i16;
type I32Func = unsafe extern "C" fn() -> i32;
type I64Func = unsafe extern "C" fn() -> i64;
type FloatFunc = unsafe extern "C" fn() -> f32;
type DoubleFunc = unsafe extern "C" fn() -> f64;

// JIT function type for vector returns (takes output pointer)
type VecFunc = unsafe extern "C" fn(*mut u8);

pub struct JitEngine<'ctx> {
    pub context: &'ctx Context,
}

impl<'ctx> JitEngine<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self { context }
    }

    /// Compile and evaluate a function, calling it with the given arguments
    pub fn eval_function(&self, func: &FunctionDef, args: &[Value]) -> Result<Value> {
        // Type check the function
        let checker = TypeChecker::new();
        checker.check_function(func)?;

        // Create codegen and compile
        let codegen = CodeGen::new(self.context, "lir_func");
        codegen.compile_function(func)?;

        // Create execution engine
        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Call based on signature
        self.call_compiled_function(&execution_engine, &func.name, &func.return_type, args)
    }

    /// Call a compiled function by name (public for test harness)
    pub fn call_compiled_function(
        &self,
        execution_engine: &inkwell::execution_engine::ExecutionEngine<'ctx>,
        name: &str,
        return_type: &ReturnType,
        args: &[Value],
    ) -> Result<Value> {
        // For now, we support specific signatures. This is a simplified implementation.
        match (return_type, args.len()) {
            // No args
            (ReturnType::Scalar(ScalarType::I32), 0) => {
                let func: JitFunction<I32Func> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I32(result))
            }
            (ReturnType::Scalar(ScalarType::I64), 0) => {
                let func: JitFunction<I64Func> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I64(result))
            }
            (ReturnType::Scalar(ScalarType::I8), 0) => {
                let func: JitFunction<I8Func> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I8(result))
            }
            (ReturnType::Scalar(ScalarType::I1), 0) => {
                let func: JitFunction<I1Func> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I1(result))
            }
            (ReturnType::Scalar(ScalarType::Float), 0) => {
                let func: JitFunction<FloatFunc> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Float(result))
            }
            (ReturnType::Scalar(ScalarType::Double), 0) => {
                let func: JitFunction<DoubleFunc> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Double(result))
            }
            (ReturnType::Scalar(ScalarType::Void), 0) => {
                type VoidFunc = unsafe extern "C" fn();
                let func: JitFunction<VoidFunc> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                unsafe { func.call() };
                Ok(Value::I32(0)) // Return 0 for void functions (success indicator)
            }
            (ReturnType::Ptr, 0) => {
                type PtrFunc = unsafe extern "C" fn() -> *const u8;
                let func: JitFunction<PtrFunc> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Ptr(result as u64))
            }

            // Ptr return with i64 arg (common for make_adder pattern)
            (ReturnType::Ptr, 1) => {
                type I64ToPtr = unsafe extern "C" fn(i64) -> *const u8;
                let func: JitFunction<I64ToPtr> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg = match args[0] {
                    Value::I64(v) => v,
                    Value::Ptr(v) => v as i64,
                    _ => {
                        return Err(CodeGenError::CodeGen(
                            "expected i64 or ptr argument".to_string(),
                        ))
                    }
                };
                let result = unsafe { func.call(arg) };
                Ok(Value::Ptr(result as u64))
            }

            // Ptr return with two i64 args
            (ReturnType::Ptr, 2) => {
                type I64I64ToPtr = unsafe extern "C" fn(i64, i64) -> *const u8;
                let func: JitFunction<I64I64ToPtr> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg1 = match args[0] {
                    Value::I64(v) => v,
                    Value::Ptr(v) => v as i64,
                    _ => {
                        return Err(CodeGenError::CodeGen(
                            "expected i64 or ptr argument".to_string(),
                        ))
                    }
                };
                let arg2 = match args[1] {
                    Value::I64(v) => v,
                    Value::Ptr(v) => v as i64,
                    _ => {
                        return Err(CodeGenError::CodeGen(
                            "expected i64 or ptr argument".to_string(),
                        ))
                    }
                };
                let result = unsafe { func.call(arg1, arg2) };
                Ok(Value::Ptr(result as u64))
            }

            // One i32 arg
            (ReturnType::Scalar(ScalarType::I32), 1) => {
                type I32ToI32 = unsafe extern "C" fn(i32) -> i32;
                let func: JitFunction<I32ToI32> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg = match args[0] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let result = unsafe { func.call(arg) };
                Ok(Value::I32(result))
            }

            (ReturnType::Scalar(ScalarType::I1), 1) => {
                type I32ToI1 = unsafe extern "C" fn(i32) -> bool;
                let func: JitFunction<I32ToI1> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg = match args[0] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let result = unsafe { func.call(arg) };
                Ok(Value::I1(result))
            }

            // One double arg
            (ReturnType::Scalar(ScalarType::Double), 1) => {
                type DoubleToDouble = unsafe extern "C" fn(f64) -> f64;
                let func: JitFunction<DoubleToDouble> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg = match args[0] {
                    Value::Double(v) => v,
                    _ => {
                        return Err(CodeGenError::CodeGen(
                            "expected double argument".to_string(),
                        ))
                    }
                };
                let result = unsafe { func.call(arg) };
                Ok(Value::Double(result))
            }

            // Two i32 args
            (ReturnType::Scalar(ScalarType::I32), 2) => {
                type I32I32ToI32 = unsafe extern "C" fn(i32, i32) -> i32;
                let func: JitFunction<I32I32ToI32> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg1 = match args[0] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let arg2 = match args[1] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let result = unsafe { func.call(arg1, arg2) };
                Ok(Value::I32(result))
            }

            // Two args returning double (for int-to-float-add pattern)
            (ReturnType::Scalar(ScalarType::Double), 2) => {
                type I32I32ToDouble = unsafe extern "C" fn(i32, i32) -> f64;
                let func: JitFunction<I32I32ToDouble> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg1 = match args[0] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let arg2 = match args[1] {
                    Value::I32(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i32 argument".to_string())),
                };
                let result = unsafe { func.call(arg1, arg2) };
                Ok(Value::Double(result))
            }

            // i64 functions with args
            (ReturnType::Scalar(ScalarType::I64), 1) => {
                type I64ToI64 = unsafe extern "C" fn(i64) -> i64;
                let func: JitFunction<I64ToI64> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg = match args[0] {
                    Value::I64(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i64 argument".to_string())),
                };
                let result = unsafe { func.call(arg) };
                Ok(Value::I64(result))
            }

            (ReturnType::Scalar(ScalarType::I64), 2) => {
                type I64I64ToI64 = unsafe extern "C" fn(i64, i64) -> i64;
                let func: JitFunction<I64I64ToI64> = unsafe {
                    execution_engine
                        .get_function(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let arg1 = match args[0] {
                    Value::I64(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i64 argument".to_string())),
                };
                let arg2 = match args[1] {
                    Value::I64(v) => v,
                    _ => return Err(CodeGenError::CodeGen("expected i64 argument".to_string())),
                };
                let result = unsafe { func.call(arg1, arg2) };
                Ok(Value::I64(result))
            }

            _ => Err(CodeGenError::CodeGen(format!(
                "unsupported function signature: {:?}({} args)",
                return_type,
                args.len()
            ))),
        }
    }

    /// Evaluate an expression using JIT compilation
    pub fn eval(&self, expr: &Expr) -> Result<Value> {
        // Type check to get result type
        let mut checker = TypeChecker::new();
        let ty = checker.check(expr)?;

        // Create codegen and compile
        let codegen = CodeGen::new(self.context, "lir_eval");
        codegen.create_eval_function(expr, &ty)?;

        // Create execution engine
        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Execute based on return type
        match ty {
            Type::Scalar(ScalarType::I1) => {
                let func: JitFunction<I1Func> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I1(result))
            }
            Type::Scalar(ScalarType::I8) => {
                let func: JitFunction<I8Func> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I8(result))
            }
            Type::Scalar(ScalarType::I16) => {
                let func: JitFunction<I16Func> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I16(result))
            }
            Type::Scalar(ScalarType::I32) => {
                let func: JitFunction<I32Func> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I32(result))
            }
            Type::Scalar(ScalarType::I64) => {
                let func: JitFunction<I64Func> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I64(result))
            }
            Type::Scalar(ScalarType::Float) => {
                let func: JitFunction<FloatFunc> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Float(result))
            }
            Type::Scalar(ScalarType::Double) => {
                let func: JitFunction<DoubleFunc> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Double(result))
            }
            Type::Vector(vt) => self.eval_vector(&execution_engine, &vt),
            Type::Scalar(ScalarType::Void) => Err(CodeGenError::CodeGen(
                "cannot eval void expression".to_string(),
            )),
            Type::Ptr => {
                type PtrFunc = unsafe extern "C" fn() -> *const u8;
                let func: JitFunction<PtrFunc> = unsafe {
                    execution_engine
                        .get_function("__lir_eval")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Ptr(result as u64))
            }
        }
    }

    /// Evaluate a vector-returning expression
    fn eval_vector(
        &self,
        execution_engine: &inkwell::execution_engine::ExecutionEngine<'ctx>,
        vt: &VectorType,
    ) -> Result<Value> {
        let func: JitFunction<VecFunc> = unsafe {
            execution_engine
                .get_function("__lir_eval")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let count = vt.count as usize;

        // Allocate buffer for vector result based on element type
        match vt.element {
            ScalarType::I1 => {
                let mut buf: Vec<u8> = vec![0; count]; // i1 stored as i8
                unsafe {
                    func.call(buf.as_mut_ptr());
                }
                Ok(Value::VecI1(buf.into_iter().map(|v| v != 0).collect()))
            }
            ScalarType::I8 => {
                let mut buf: Vec<i8> = vec![0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecI8(buf))
            }
            ScalarType::I16 => {
                let mut buf: Vec<i16> = vec![0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecI16(buf))
            }
            ScalarType::I32 => {
                let mut buf: Vec<i32> = vec![0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecI32(buf))
            }
            ScalarType::I64 => {
                let mut buf: Vec<i64> = vec![0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecI64(buf))
            }
            ScalarType::Float => {
                let mut buf: Vec<f32> = vec![0.0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecFloat(buf))
            }
            ScalarType::Double => {
                let mut buf: Vec<f64> = vec![0.0; count];
                unsafe {
                    func.call(buf.as_mut_ptr() as *mut u8);
                }
                Ok(Value::VecDouble(buf))
            }
            ScalarType::Void => Err(CodeGenError::CodeGen(
                "cannot have void element type in vector".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lir_core::ast::{BasicBlock, FCmpPred, FloatValue, ICmpPred, ScalarType};

    #[test]
    fn test_integer_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::IntLit {
            ty: ScalarType::I32,
            value: 42,
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_add() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Add(
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 7,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(12));
    }

    #[test]
    fn test_sub() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Sub(
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 10,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 3,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(7));
    }

    #[test]
    fn test_mul() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Mul(
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 6,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 7,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -10,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 3,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-3));
    }

    #[test]
    fn test_udiv() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::UDiv(
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 10,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 3,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -10,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 3,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-1));
    }

    #[test]
    fn test_urem() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::URem(
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 10,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 3,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I8,
                value: 127,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I8,
                value: 1,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I8(-128));
    }

    #[test]
    fn test_i64() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::Add(
            Box::new(Expr::IntLit {
                ty: ScalarType::I64,
                value: 1_000_000_000_000,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I64,
                value: 1,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I64(1_000_000_000_001));
    }

    #[test]
    fn test_i1() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::IntLit {
            ty: ScalarType::I1,
            value: 1,
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    // Float tests

    #[test]
    fn test_double_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit {
            ty: ScalarType::Double,
            value: FloatValue::Number(3.25),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(3.25));
    }

    #[test]
    fn test_float_literal() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit {
            ty: ScalarType::Float,
            value: FloatValue::Number(2.5),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Float(2.5));
    }

    #[test]
    fn test_fadd() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FAdd(
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(5.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(6.0),
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(11.0));
    }

    #[test]
    fn test_fsub() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FSub(
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(10.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(3.0),
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(7.0));
    }

    #[test]
    fn test_fmul() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FMul(
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(6.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(7.0),
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(42.0));
    }

    #[test]
    fn test_fdiv() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FDiv(
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(10.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(4.0),
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(2.5));
    }

    #[test]
    fn test_frem() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FRem(
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(10.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(3.0),
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(1.0));
    }

    #[test]
    fn test_float_inf() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FloatLit {
            ty: ScalarType::Double,
            value: FloatValue::Inf,
        };
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

        let expr = Expr::FloatLit {
            ty: ScalarType::Double,
            value: FloatValue::NegInf,
        };
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

        let expr = Expr::FloatLit {
            ty: ScalarType::Double,
            value: FloatValue::Nan,
        };
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
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
            Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(0.0),
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1100,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1010,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1100,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1010,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1100,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 0b1010,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 4,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 16,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 16,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -8,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -1,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 1,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 0,
            }),
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
            Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 1,
            }),
            Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 0,
            }),
        );
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    // ICmp tests

    #[test]
    fn test_icmp_eq() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::ICmp {
            pred: ICmpPred::Eq,
            lhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
            rhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_icmp_ne() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::ICmp {
            pred: ICmpPred::Ne,
            lhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
            rhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 6,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_icmp_slt() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Signed: -1 < 1
        let expr = Expr::ICmp {
            pred: ICmpPred::Slt,
            lhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -1,
            }),
            rhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_icmp_ult() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Unsigned: -1 (0xFFFFFFFF) > 1, so ult is false
        let expr = Expr::ICmp {
            pred: ICmpPred::Ult,
            lhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -1,
            }),
            rhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(false));
    }

    #[test]
    fn test_icmp_sge() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::ICmp {
            pred: ICmpPred::Sge,
            lhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
            rhs: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 5,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    // FCmp tests

    #[test]
    fn test_fcmp_oeq() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FCmp {
            pred: FCmpPred::Oeq,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_fcmp_olt() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        let expr = Expr::FCmp {
            pred: FCmpPred::Olt,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(2.0),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_fcmp_uno() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // uno is true if either is NaN
        let expr = Expr::FCmp {
            pred: FCmpPred::Uno,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Nan,
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_fcmp_ord() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // ord is true if neither is NaN
        let expr = Expr::FCmp {
            pred: FCmpPred::Ord,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(1.0),
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(2.0),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    #[test]
    fn test_fcmp_oeq_nan() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Ordered comparisons return false when NaN is involved
        let expr = Expr::FCmp {
            pred: FCmpPred::Oeq,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Nan,
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Nan,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(false));
    }

    #[test]
    fn test_fcmp_ueq_nan() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // Unordered eq returns true if either is NaN
        let expr = Expr::FCmp {
            pred: FCmpPred::Ueq,
            lhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Nan,
            }),
            rhs: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Nan,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I1(true));
    }

    // Conversion tests

    #[test]
    fn test_trunc() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i32 258 truncated to i8 = 2 (loses high bits)
        let expr = Expr::Trunc {
            ty: ScalarType::I8,
            value: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 258,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I8(2));
    }

    #[test]
    fn test_zext() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i8 200 zero-extended to i32 = 200 (not sign extended to negative)
        let expr = Expr::ZExt {
            ty: ScalarType::I32,
            value: Box::new(Expr::IntLit {
                ty: ScalarType::I8,
                value: 200_u8 as i128,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(200));
    }

    #[test]
    fn test_sext() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i8 -1 sign-extended to i32 = -1
        let expr = Expr::SExt {
            ty: ScalarType::I32,
            value: Box::new(Expr::IntLit {
                ty: ScalarType::I8,
                value: -1,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-1));
    }

    #[test]
    fn test_fptrunc() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // double to float
        let expr = Expr::FPTrunc {
            ty: ScalarType::Float,
            value: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(3.25),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Float(v) => assert!((v - 3.25_f32).abs() < 0.001),
            _ => panic!("expected Float"),
        }
    }

    #[test]
    fn test_fpext() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // float to double
        let expr = Expr::FPExt {
            ty: ScalarType::Double,
            value: Box::new(Expr::FloatLit {
                ty: ScalarType::Float,
                value: FloatValue::Number(3.25),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        match result {
            Value::Double(v) => assert!((v - 3.25).abs() < 0.001),
            _ => panic!("expected Double"),
        }
    }

    #[test]
    fn test_fptoui() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // double 42.7 to i32 = 42
        let expr = Expr::FPToUI {
            ty: ScalarType::I32,
            value: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(42.7),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_fptosi() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // double -42.7 to i32 = -42
        let expr = Expr::FPToSI {
            ty: ScalarType::I32,
            value: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(-42.7),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(-42));
    }

    #[test]
    fn test_uitofp() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i32 42 to double = 42.0
        let expr = Expr::UIToFP {
            ty: ScalarType::Double,
            value: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 42,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(42.0));
    }

    #[test]
    fn test_sitofp() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // i32 -42 to double = -42.0
        let expr = Expr::SIToFP {
            ty: ScalarType::Double,
            value: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: -42,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(-42.0));
    }

    // Select tests

    #[test]
    fn test_select_true() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // select true 1 2 = 1
        let expr = Expr::Select {
            cond: Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 1,
            }),
            true_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
            false_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(1));
    }

    #[test]
    fn test_select_false() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // select false 1 2 = 2
        let expr = Expr::Select {
            cond: Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 0,
            }),
            true_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
            false_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(2));
    }

    #[test]
    fn test_select_with_icmp() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // select (icmp slt 5 10) 1 2 = 1
        let expr = Expr::Select {
            cond: Box::new(Expr::ICmp {
                pred: ICmpPred::Slt,
                lhs: Box::new(Expr::IntLit {
                    ty: ScalarType::I32,
                    value: 5,
                }),
                rhs: Box::new(Expr::IntLit {
                    ty: ScalarType::I32,
                    value: 10,
                }),
            }),
            true_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 1,
            }),
            false_val: Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 2,
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::I32(1));
    }

    #[test]
    fn test_select_float() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // select true 3.25 2.75 = 3.25
        let expr = Expr::Select {
            cond: Box::new(Expr::IntLit {
                ty: ScalarType::I1,
                value: 1,
            }),
            true_val: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(3.25),
            }),
            false_val: Box::new(Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(2.75),
            }),
        };
        let result = jit.eval(&expr).unwrap();
        assert_eq!(result, Value::Double(3.25));
    }

    // Function tests
    use lir_core::ast::Param;

    /// Helper to create a single entry block with given instructions
    fn entry_block(instructions: Vec<Expr>) -> Vec<BasicBlock> {
        vec![BasicBlock {
            label: "entry".to_string(),
            instructions,
        }]
    }

    #[test]
    fn test_function_no_params() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // (define (get-42 i32) () (block entry (ret (i32 42))))
        let func = FunctionDef {
            name: "get-42".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 42,
            })))]),
        };

        let result = jit.eval_function(&func, &[]).unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_function_one_param() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // (define (add-one i32) ((i32 x)) (block entry (ret (add x (i32 1)))))
        let func = FunctionDef {
            name: "add-one".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![Param {
                ty: ParamType::Scalar(ScalarType::I32),
                name: "x".to_string(),
            }],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::Add(
                Box::new(Expr::LocalRef("x".to_string())),
                Box::new(Expr::IntLit {
                    ty: ScalarType::I32,
                    value: 1,
                }),
            ))))]),
        };

        let result = jit.eval_function(&func, &[Value::I32(5)]).unwrap();
        assert_eq!(result, Value::I32(6));
    }

    #[test]
    fn test_function_two_params() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // (define (add-two i32) ((i32 a) (i32 b)) (block entry (ret (add a b))))
        let func = FunctionDef {
            name: "add-two".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![
                Param {
                    ty: ParamType::Scalar(ScalarType::I32),
                    name: "a".to_string(),
                },
                Param {
                    ty: ParamType::Scalar(ScalarType::I32),
                    name: "b".to_string(),
                },
            ],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::Add(
                Box::new(Expr::LocalRef("a".to_string())),
                Box::new(Expr::LocalRef("b".to_string())),
            ))))]),
        };

        let result = jit
            .eval_function(&func, &[Value::I32(3), Value::I32(4)])
            .unwrap();
        assert_eq!(result, Value::I32(7));
    }

    #[test]
    fn test_function_with_computation() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // (define (square i32) ((i32 x)) (block entry (ret (mul x x))))
        let func = FunctionDef {
            name: "square".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![Param {
                ty: ParamType::Scalar(ScalarType::I32),
                name: "x".to_string(),
            }],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::Mul(
                Box::new(Expr::LocalRef("x".to_string())),
                Box::new(Expr::LocalRef("x".to_string())),
            ))))]),
        };

        let result = jit.eval_function(&func, &[Value::I32(7)]).unwrap();
        assert_eq!(result, Value::I32(49));
    }

    #[test]
    fn test_function_with_icmp_select() {
        let context = Context::create();
        let jit = JitEngine::new(&context);

        // (define (max i32) ((i32 a) (i32 b)) (block entry (ret (select (icmp sgt a b) a b))))
        let func = FunctionDef {
            name: "max".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![
                Param {
                    ty: ParamType::Scalar(ScalarType::I32),
                    name: "a".to_string(),
                },
                Param {
                    ty: ParamType::Scalar(ScalarType::I32),
                    name: "b".to_string(),
                },
            ],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::Select {
                cond: Box::new(Expr::ICmp {
                    pred: ICmpPred::Sgt,
                    lhs: Box::new(Expr::LocalRef("a".to_string())),
                    rhs: Box::new(Expr::LocalRef("b".to_string())),
                }),
                true_val: Box::new(Expr::LocalRef("a".to_string())),
                false_val: Box::new(Expr::LocalRef("b".to_string())),
            })))]),
        };

        let result = jit
            .eval_function(&func, &[Value::I32(10), Value::I32(20)])
            .unwrap();
        assert_eq!(result, Value::I32(20));

        let result2 = jit
            .eval_function(&func, &[Value::I32(30), Value::I32(5)])
            .unwrap();
        assert_eq!(result2, Value::I32(30));
    }
}
