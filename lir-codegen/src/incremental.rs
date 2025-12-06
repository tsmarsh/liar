//! Incremental JIT compilation
//!
//! Provides a session-based JIT compiler that supports incremental
//! compilation of functions. New definitions can be added without
//! recompiling everything.
//!
//! This implementation uses MCJIT's add_module capability. While not
//! as efficient as true OrcJIT, it provides the same API and can be
//! swapped out later if needed.

use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use lir_core::ast::{FunctionDef, ParamType, ReturnType, ScalarType};
use std::collections::HashMap;

use crate::codegen::{CodeGen, CodeGenError, Value};

/// Information about a compiled symbol
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
}

/// The kind of symbol
#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function {
        return_type: ReturnType,
        param_count: usize,
    },
}

/// Incremental JIT compiler
///
/// Supports adding new function definitions incrementally without
/// recompiling existing code.
pub struct IncrementalJit<'ctx> {
    context: &'ctx Context,
    /// The main execution engine
    execution_engine: ExecutionEngine<'ctx>,
    /// Track defined symbols
    symbols: HashMap<String, SymbolInfo>,
    /// Counter for unique module names
    module_counter: u64,
    /// Counter for unique eval thunk names
    eval_counter: u64,
}

impl<'ctx> IncrementalJit<'ctx> {
    /// Create a new incremental JIT compiler
    pub fn new(context: &'ctx Context) -> Result<Self, String> {
        // Create initial module to bootstrap the execution engine
        let module = context.create_module("__init");

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| format!("Failed to create JIT: {}", e))?;

        Ok(Self {
            context,
            execution_engine,
            symbols: HashMap::new(),
            module_counter: 0,
            eval_counter: 0,
        })
    }

    /// Get the context
    pub fn context(&self) -> &'ctx Context {
        self.context
    }

    /// Check if a symbol is defined
    pub fn has_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Get symbol info
    pub fn get_symbol(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbols.get(name)
    }

    /// Get all defined symbols
    pub fn symbols(&self) -> impl Iterator<Item = &SymbolInfo> {
        self.symbols.values()
    }

    /// Add a function definition to the JIT
    ///
    /// The function will be compiled and its symbol will be available
    /// for future compilations to reference.
    pub fn define_function(&mut self, func: &FunctionDef) -> Result<(), CodeGenError> {
        // Create a new module for this function
        self.module_counter += 1;
        let module_name = format!("__func_{}", self.module_counter);
        let module = self.context.create_module(&module_name);

        // Declare any external functions this might call
        self.declare_externals(&module);

        // Compile the function
        let codegen = CodeGen::with_module(self.context, module);
        codegen.compile_function(func)?;

        // Add to execution engine
        self.execution_engine
            .add_module(&codegen.module)
            .map_err(|_| {
                CodeGenError::CodeGen("Failed to add module to execution engine".to_string())
            })?;

        // Track the symbol
        self.symbols.insert(
            func.name.clone(),
            SymbolInfo {
                name: func.name.clone(),
                kind: SymbolKind::Function {
                    return_type: func.return_type.clone(),
                    param_count: func.params.len(),
                },
            },
        );

        Ok(())
    }

    /// Add multiple functions at once (for mutual recursion)
    pub fn define_functions(&mut self, funcs: &[&FunctionDef]) -> Result<(), CodeGenError> {
        // Create a new module for all functions
        self.module_counter += 1;
        let module_name = format!("__funcs_{}", self.module_counter);
        let module = self.context.create_module(&module_name);

        // Declare any external functions
        self.declare_externals(&module);

        let codegen = CodeGen::with_module(self.context, module);

        // First pass: declare all functions
        for func in funcs {
            codegen.declare_function(func);
        }

        // Second pass: compile all functions
        for func in funcs {
            codegen.compile_function(func)?;
        }

        // Add to execution engine
        self.execution_engine
            .add_module(&codegen.module)
            .map_err(|_| {
                CodeGenError::CodeGen("Failed to add module to execution engine".to_string())
            })?;

        // Track all symbols
        for func in funcs {
            self.symbols.insert(
                func.name.clone(),
                SymbolInfo {
                    name: func.name.clone(),
                    kind: SymbolKind::Function {
                        return_type: func.return_type.clone(),
                        param_count: func.params.len(),
                    },
                },
            );
        }

        Ok(())
    }

    /// Declare external symbols so new modules can call previously defined functions
    fn declare_externals(&self, module: &Module<'ctx>) {
        // For each symbol we have, declare it as external in the new module
        // This allows LLVM to resolve calls across module boundaries
        for (name, info) in &self.symbols {
            match &info.kind {
                SymbolKind::Function {
                    return_type,
                    param_count,
                } => {
                    // Build a stub declaration for the function
                    // Since we don't have param types stored, we'll use a generic approach
                    // The actual linking happens via the execution engine
                    let _ = self.declare_external_function(module, name, return_type, *param_count);
                }
            }
        }
    }

    /// Declare an external function in a module
    fn declare_external_function(
        &self,
        module: &Module<'ctx>,
        name: &str,
        return_type: &ReturnType,
        _param_count: usize,
    ) -> inkwell::values::FunctionValue<'ctx> {
        // Check if already declared
        if let Some(existing) = module.get_function(name) {
            return existing;
        }

        // For now, use a simple i64-based declaration
        // This works because LLVM's JIT uses the actual function addresses
        // and doesn't type-check across module boundaries at link time
        let param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = vec![];

        let fn_type = match return_type {
            ReturnType::Scalar(ScalarType::Void) => {
                self.context.void_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::I1) => {
                self.context.bool_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::I8) => {
                self.context.i8_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::I16) => {
                self.context.i16_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::I32) => {
                self.context.i32_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::I64) => {
                self.context.i64_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::Float) => {
                self.context.f32_type().fn_type(&param_types, true)
            }
            ReturnType::Scalar(ScalarType::Double) => {
                self.context.f64_type().fn_type(&param_types, true)
            }
            ReturnType::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(&param_types, true),
            ReturnType::AnonStruct(fields) => {
                let field_types: Vec<inkwell::types::BasicTypeEnum> = fields
                    .iter()
                    .filter_map(|f| match f {
                        ParamType::Scalar(s) => match s {
                            ScalarType::I1 => Some(self.context.bool_type().into()),
                            ScalarType::I8 => Some(self.context.i8_type().into()),
                            ScalarType::I16 => Some(self.context.i16_type().into()),
                            ScalarType::I32 => Some(self.context.i32_type().into()),
                            ScalarType::I64 => Some(self.context.i64_type().into()),
                            ScalarType::Float => Some(self.context.f32_type().into()),
                            ScalarType::Double => Some(self.context.f64_type().into()),
                            ScalarType::Void => None,
                        },
                        ParamType::Ptr
                        | ParamType::Own(_)
                        | ParamType::Ref(_)
                        | ParamType::RefMut(_)
                        | ParamType::Rc(_) => Some(
                            self.context
                                .ptr_type(inkwell::AddressSpace::default())
                                .into(),
                        ),
                        ParamType::AnonStruct(inner_fields) => {
                            // Nested anonymous structs - just use ptr for now
                            let field_types: Vec<inkwell::types::BasicTypeEnum> = inner_fields
                                .iter()
                                .map(|_| {
                                    self.context
                                        .ptr_type(inkwell::AddressSpace::default())
                                        .into()
                                })
                                .collect();
                            Some(self.context.struct_type(&field_types, false).into())
                        }
                    })
                    .collect();
                let struct_type = self.context.struct_type(&field_types, false);
                struct_type.fn_type(&param_types, true)
            }
        };

        module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    /// Call a no-arg function by name
    pub fn call_function(&self, name: &str) -> Result<Value, CodeGenError> {
        let info = self
            .symbols
            .get(name)
            .ok_or_else(|| CodeGenError::CodeGen(format!("function '{}' not found", name)))?;

        let return_type = match &info.kind {
            SymbolKind::Function { return_type, .. } => return_type.clone(),
        };

        self.call_function_typed(name, &return_type)
    }

    /// Call a function by name with known return type
    fn call_function_typed(
        &self,
        name: &str,
        return_type: &ReturnType,
    ) -> Result<Value, CodeGenError> {
        match return_type {
            ReturnType::Scalar(ScalarType::I32) => {
                type F = unsafe extern "C" fn() -> i32;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I32(result))
            }
            ReturnType::Scalar(ScalarType::I64) => {
                type F = unsafe extern "C" fn() -> i64;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I64(result))
            }
            ReturnType::Scalar(ScalarType::I8) => {
                type F = unsafe extern "C" fn() -> i8;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I8(result))
            }
            ReturnType::Scalar(ScalarType::I16) => {
                type F = unsafe extern "C" fn() -> i16;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I16(result))
            }
            ReturnType::Scalar(ScalarType::I1) => {
                type F = unsafe extern "C" fn() -> bool;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::I1(result))
            }
            ReturnType::Scalar(ScalarType::Float) => {
                type F = unsafe extern "C" fn() -> f32;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Float(result))
            }
            ReturnType::Scalar(ScalarType::Double) => {
                type F = unsafe extern "C" fn() -> f64;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Double(result))
            }
            ReturnType::Scalar(ScalarType::Void) => {
                type F = unsafe extern "C" fn();
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                unsafe { func.call() };
                Ok(Value::I32(0))
            }
            ReturnType::Ptr => {
                type F = unsafe extern "C" fn() -> *const u8;
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let result = unsafe { func.call() };
                Ok(Value::Ptr(result as u64))
            }
            ReturnType::AnonStruct(_) => {
                // For struct returns, we can't easily return the value
                // Just return the first field as ptr (assumes closure struct)
                #[allow(improper_ctypes_definitions)]
                type F = unsafe extern "C" fn() -> (*const u8, *const u8);
                let func = unsafe {
                    self.execution_engine
                        .get_function::<F>(name)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                let (fn_ptr, _env_ptr) = unsafe { func.call() };
                // Return as a special struct representation
                // For now, just return the first field (fn_ptr)
                Ok(Value::Ptr(fn_ptr as u64))
            }
        }
    }

    /// Create and call an evaluation thunk
    ///
    /// The expression is wrapped in a no-arg function, compiled, and called.
    pub fn eval_thunk(&mut self, func: &FunctionDef) -> Result<Value, CodeGenError> {
        // Create a new module for the thunk
        self.eval_counter += 1;
        let module_name = format!("__eval_{}", self.eval_counter);
        let module = self.context.create_module(&module_name);

        // Declare externals
        self.declare_externals(&module);

        // Compile the thunk
        let codegen = CodeGen::with_module(self.context, module);
        codegen.compile_function(func)?;

        // Add to execution engine
        self.execution_engine
            .add_module(&codegen.module)
            .map_err(|_| {
                CodeGenError::CodeGen("Failed to add eval module to execution engine".to_string())
            })?;

        // Call and return
        self.call_function_typed(&func.name, &func.return_type)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lir_core::ast::{BasicBlock, Expr};

    fn entry_block(instructions: Vec<Expr>) -> Vec<BasicBlock> {
        vec![BasicBlock {
            label: "entry".to_string(),
            instructions,
        }]
    }

    #[test]
    fn test_define_and_call() {
        let context = Context::create();
        let mut jit = IncrementalJit::new(&context).unwrap();

        // Define a simple function: (define (get42 i32) () (block entry (ret (i32 42))))
        let func = FunctionDef {
            name: "get42".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 42,
            })))]),
        };

        jit.define_function(&func).unwrap();
        assert!(jit.has_symbol("get42"));

        let result = jit.call_function("get42").unwrap();
        assert_eq!(result, Value::I32(42));
    }

    #[test]
    fn test_multiple_definitions() {
        let context = Context::create();
        let mut jit = IncrementalJit::new(&context).unwrap();

        // Define first function
        let func1 = FunctionDef {
            name: "ten".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 10,
            })))]),
        };

        jit.define_function(&func1).unwrap();

        // Define second function
        let func2 = FunctionDef {
            name: "twenty".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::IntLit {
                ty: ScalarType::I32,
                value: 20,
            })))]),
        };

        jit.define_function(&func2).unwrap();

        // Both should be callable
        assert_eq!(jit.call_function("ten").unwrap(), Value::I32(10));
        assert_eq!(jit.call_function("twenty").unwrap(), Value::I32(20));
    }

    #[test]
    fn test_eval_thunk() {
        let context = Context::create();
        let mut jit = IncrementalJit::new(&context).unwrap();

        // Create an eval thunk: () -> i32 { ret (add 5 7) }
        let thunk = FunctionDef {
            name: "__eval_test".to_string(),
            return_type: ReturnType::Scalar(ScalarType::I32),
            params: vec![],
            blocks: entry_block(vec![Expr::Ret(Some(Box::new(Expr::Add(
                Box::new(Expr::IntLit {
                    ty: ScalarType::I32,
                    value: 5,
                }),
                Box::new(Expr::IntLit {
                    ty: ScalarType::I32,
                    value: 7,
                }),
            ))))]),
        };

        let result = jit.eval_thunk(&thunk).unwrap();
        assert_eq!(result, Value::I32(12));
    }
}
