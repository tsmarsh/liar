//! LLVM IR code generation

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, IntType, VectorType as LLVMVectorType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, VectorValue as LLVMVectorValue,
};

use inkwell::{FloatPredicate, IntPredicate};
use lir_core::ast::{
    BranchTarget, Expr, ExternDecl, FCmpPred, FloatValue, FunctionDef, GlobalDef, ICmpPred,
    ParamType, ReturnType, ScalarType, Type, VectorType,
};
use lir_core::error::TypeError;
use lir_core::types::TypeChecker;
use std::collections::HashMap;

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

    /// Get LLVM vector type
    fn vec_type(&self, ty: &VectorType) -> LLVMVectorType<'ctx> {
        if ty.element.is_integer() {
            self.int_type(&ty.element).vec_type(ty.count)
        } else {
            match ty.element {
                ScalarType::Float => self.context.f32_type().vec_type(ty.count),
                ScalarType::Double => self.context.f64_type().vec_type(ty.count),
                _ => unreachable!(),
            }
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
            Type::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
        }
    }

    /// Get LLVM type from ScalarType (for function signatures)
    fn scalar_to_basic_type(&self, ty: &ScalarType) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            ScalarType::I1 => Some(self.context.bool_type().into()),
            ScalarType::I8 => Some(self.context.i8_type().into()),
            ScalarType::I16 => Some(self.context.i16_type().into()),
            ScalarType::I32 => Some(self.context.i32_type().into()),
            ScalarType::I64 => Some(self.context.i64_type().into()),
            ScalarType::Float => Some(self.context.f32_type().into()),
            ScalarType::Double => Some(self.context.f64_type().into()),
            ScalarType::Void => None,
        }
    }

    /// Compile a function definition
    pub fn compile_function(&self, func: &FunctionDef) -> Result<FunctionValue<'ctx>> {
        // Build parameter types
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func
            .params
            .iter()
            .filter_map(|p| self.scalar_to_basic_type(&p.ty))
            .map(|t| t.into())
            .collect();

        // Build function type
        let fn_type = match &func.return_type {
            ScalarType::Void => self.context.void_type().fn_type(&param_types, false),
            ScalarType::I1 => self.context.bool_type().fn_type(&param_types, false),
            ScalarType::I8 => self.context.i8_type().fn_type(&param_types, false),
            ScalarType::I16 => self.context.i16_type().fn_type(&param_types, false),
            ScalarType::I32 => self.context.i32_type().fn_type(&param_types, false),
            ScalarType::I64 => self.context.i64_type().fn_type(&param_types, false),
            ScalarType::Float => self.context.f32_type().fn_type(&param_types, false),
            ScalarType::Double => self.context.f64_type().fn_type(&param_types, false),
        };

        // Create function
        let function = self.module.add_function(&func.name, fn_type, None);

        // Create all basic blocks first (for forward references in branches)
        let mut block_map: HashMap<String, inkwell::basic_block::BasicBlock<'ctx>> = HashMap::new();
        for block in &func.blocks {
            let bb = self.context.append_basic_block(function, &block.label);
            block_map.insert(block.label.clone(), bb);
        }

        // Build locals map from parameters
        let mut locals: HashMap<String, BasicValueEnum<'ctx>> = HashMap::new();
        for (i, param) in func.params.iter().enumerate() {
            let param_value = function.get_nth_param(i as u32).unwrap();
            locals.insert(param.name.clone(), param_value);
        }

        // Compile each block
        for block in &func.blocks {
            let bb = block_map[&block.label];
            self.builder.position_at_end(bb);

            for expr in &block.instructions {
                self.compile_expr_with_block_context(expr, &locals, &block_map)?;
            }
        }

        Ok(function)
    }

    /// Compile an external function declaration
    pub fn compile_extern_decl(&self, decl: &ExternDecl) -> Result<FunctionValue<'ctx>> {
        // Build parameter types
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = decl
            .param_types
            .iter()
            .filter_map(|p| match p {
                ParamType::Scalar(s) => self.scalar_to_basic_type(s),
                ParamType::Ptr => Some(
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                ),
            })
            .map(|t| t.into())
            .collect();

        // Build function type
        let fn_type = match &decl.return_type {
            ReturnType::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(&param_types, decl.varargs),
            ReturnType::Scalar(ScalarType::Void) => {
                self.context.void_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::I1) => {
                self.context.bool_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::I8) => {
                self.context.i8_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::I16) => {
                self.context.i16_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::I32) => {
                self.context.i32_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::I64) => {
                self.context.i64_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::Float) => {
                self.context.f32_type().fn_type(&param_types, decl.varargs)
            }
            ReturnType::Scalar(ScalarType::Double) => {
                self.context.f64_type().fn_type(&param_types, decl.varargs)
            }
        };

        // Add external function declaration
        let function = self.module.add_function(&decl.name, fn_type, None);
        Ok(function)
    }

    /// Compile a global variable definition
    pub fn compile_global(&self, global: &GlobalDef) -> Result<inkwell::values::GlobalValue<'ctx>> {
        // Compile the initializer to get a constant value
        // For now, we only support simple literal initializers
        let (global_type, initializer): (BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>) =
            match &global.initializer {
                Expr::IntLit { ty, value } => {
                    let int_type = self.int_type(ty);
                    (
                        int_type.into(),
                        int_type.const_int(*value as u64, *value < 0).into(),
                    )
                }
                Expr::FloatLit { ty, value } => {
                    let float_val = match value {
                        FloatValue::Number(n) => *n,
                        FloatValue::Inf => f64::INFINITY,
                        FloatValue::NegInf => f64::NEG_INFINITY,
                        FloatValue::Nan => f64::NAN,
                    };
                    match ty {
                        ScalarType::Float => (
                            self.context.f32_type().into(),
                            self.context.f32_type().const_float(float_val).into(),
                        ),
                        ScalarType::Double => (
                            self.context.f64_type().into(),
                            self.context.f64_type().const_float(float_val).into(),
                        ),
                        _ => {
                            return Err(CodeGenError::CodeGen(
                                "invalid float type for global".to_string(),
                            ))
                        }
                    }
                }
                Expr::NullPtr => {
                    let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                    (ptr_type.into(), ptr_type.const_null().into())
                }
                Expr::StringLit(s) => {
                    // String literal becomes a constant [N x i8] array
                    // Use context.const_string which handles the array creation
                    let const_array = self.context.const_string(s.as_bytes(), true);
                    let array_type = const_array.get_type();
                    (array_type.into(), const_array.into())
                }
                _ => {
                    return Err(CodeGenError::NotImplemented(
                        "complex global initializers".to_string(),
                    ))
                }
            };

        // Add the global variable
        let global_val = self.module.add_global(global_type, None, &global.name);
        global_val.set_initializer(&initializer);

        // Set constant flag if specified
        global_val.set_constant(global.is_constant);

        Ok(global_val)
    }

    /// Compile expression with block context for branch targets
    fn compile_expr_with_block_context(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
        blocks: &HashMap<String, inkwell::basic_block::BasicBlock<'ctx>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            Expr::Br(target) => {
                match target {
                    BranchTarget::Unconditional(label) => {
                        let bb = blocks.get(label).ok_or_else(|| {
                            CodeGenError::CodeGen(format!("undefined block: {}", label))
                        })?;
                        self.builder
                            .build_unconditional_branch(*bb)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                    BranchTarget::Conditional {
                        cond,
                        true_label,
                        false_label,
                    } => {
                        let cond_val = self.compile_expr_recursive(cond, locals)?;
                        let cond_val = cond_val.into_int_value();
                        let true_bb = blocks.get(true_label).ok_or_else(|| {
                            CodeGenError::CodeGen(format!("undefined block: {}", true_label))
                        })?;
                        let false_bb = blocks.get(false_label).ok_or_else(|| {
                            CodeGenError::CodeGen(format!("undefined block: {}", false_label))
                        })?;
                        self.builder
                            .build_conditional_branch(cond_val, *true_bb, *false_bb)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                }
                Ok(None)
            }

            Expr::Phi { ty, incoming } => {
                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot create phi for void type".to_string())
                })?;

                let phi = self
                    .builder
                    .build_phi(llvm_ty, "phi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                for (label, value_expr) in incoming {
                    let bb = blocks.get(label).ok_or_else(|| {
                        CodeGenError::CodeGen(format!("undefined block: {}", label))
                    })?;
                    let value = self.compile_expr_recursive(value_expr, locals)?;
                    phi.add_incoming(&[(&value, *bb)]);
                }

                Ok(Some(phi.as_basic_value()))
            }

            Expr::Call { name, args } => {
                // Look up the function in the module
                let function = self.module.get_function(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined function: {}", name))
                })?;

                // Compile arguments
                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in args {
                    let val = self.compile_expr_recursive(arg, locals)?;
                    compiled_args.push(val.into());
                }

                // Build the call
                let call_site = self
                    .builder
                    .build_call(function, &compiled_args, "call")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Return the result if non-void (Basic = value, Instruction = void)
                Ok(call_site.try_as_basic_value().basic())
            }

            // Delegate to existing function for other expressions
            _ => self.compile_expr_with_locals(expr, locals),
        }
    }

    /// Compile expression with local variable context
    pub fn compile_expr_with_locals(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            // Local variable reference
            Expr::LocalRef(name) => {
                let value = locals.get(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined variable: {}", name))
                })?;
                Ok(Some(*value))
            }

            // Return instruction
            Expr::Ret(value) => {
                match value {
                    Some(v) => {
                        let val = self.compile_expr_with_locals(v, locals)?.ok_or_else(|| {
                            CodeGenError::CodeGen("ret value has no result".to_string())
                        })?;
                        self.builder
                            .build_return(Some(&val))
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                    None => {
                        self.builder
                            .build_return(None)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                }
                Ok(None) // ret doesn't produce a value
            }

            // For other expressions, delegate to the original compile_expr
            // but we need to handle sub-expressions with locals
            _ => {
                let val = self.compile_expr_recursive(expr, locals)?;
                Ok(Some(val))
            }
        }
    }

    /// Compile expression recursively with locals context
    fn compile_expr_recursive(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            // Local variable reference
            Expr::LocalRef(name) => locals
                .get(name)
                .copied()
                .ok_or_else(|| CodeGenError::CodeGen(format!("undefined variable: {}", name))),

            // Delegate literals and operations
            _ => self.compile_expr_inner(expr, locals),
        }
    }

    /// Inner expression compilation with locals support
    fn compile_expr_inner(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            // Integer literal
            Expr::IntLit {
                ty: scalar_ty,
                value,
            } => {
                let llvm_ty = self.int_type(scalar_ty);
                let val = *value as u64;
                Ok(llvm_ty
                    .const_int(val, scalar_ty != &ScalarType::I1 && *value < 0)
                    .into())
            }

            // Local variable reference
            Expr::LocalRef(name) => locals
                .get(name)
                .copied()
                .ok_or_else(|| CodeGenError::CodeGen(format!("undefined variable: {}", name))),

            // Null pointer literal
            Expr::NullPtr => {
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // String literal - creates a global constant and returns pointer
            Expr::StringLit(s) => {
                // Create a global string constant with null terminator
                let const_array = self.context.const_string(s.as_bytes(), true);
                let array_type = const_array.get_type();

                // Create a private global for this string
                let global = self.module.add_global(array_type, None, ".str");
                global.set_initializer(&const_array);
                global.set_constant(true);

                // Return pointer to the first element
                Ok(global.as_pointer_value().into())
            }

            // Binary operations - recurse with locals
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "vadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in add".to_string())),
                }
            }

            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "vsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sub".to_string())),
                }
            }

            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "vmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in mul".to_string())),
                }
            }

            Expr::SDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_div(l, r, "sdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sdiv".to_string())),
                }
            }

            Expr::UDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_div(l, r, "udiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in udiv".to_string())),
                }
            }

            Expr::SRem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_rem(l, r, "srem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in srem".to_string())),
                }
            }

            Expr::URem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_rem(l, r, "urem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in urem".to_string())),
                }
            }

            // Float literal
            Expr::FloatLit {
                ty: scalar_ty,
                value,
            } => {
                let fval = match value {
                    FloatValue::Number(n) => *n,
                    FloatValue::Inf => f64::INFINITY,
                    FloatValue::NegInf => f64::NEG_INFINITY,
                    FloatValue::Nan => f64::NAN,
                };
                match scalar_ty {
                    ScalarType::Float => Ok(self.context.f32_type().const_float(fval).into()),
                    ScalarType::Double => Ok(self.context.f64_type().const_float(fval).into()),
                    _ => Err(CodeGenError::CodeGen("invalid float type".to_string())),
                }
            }

            // Float arithmetic
            Expr::FAdd(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "fadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fadd".to_string())),
                }
            }

            Expr::FSub(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "fsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fsub".to_string())),
                }
            }

            Expr::FMul(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "fmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fmul".to_string())),
                }
            }

            Expr::FDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "fdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fdiv".to_string())),
                }
            }

            Expr::FRem(lhs, rhs) => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "frem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in frem".to_string())),
                }
            }

            // Comparisons
            Expr::ICmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
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
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_compare(llvm_pred, l, r, "icmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in icmp".to_string())),
                }
            }

            Expr::FCmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr_recursive(lhs, locals)?;
                let rhs_val = self.compile_expr_recursive(rhs, locals)?;
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
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_compare(llvm_pred, l, r, "fcmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fcmp".to_string())),
                }
            }

            // Select
            Expr::Select {
                cond,
                true_val,
                false_val,
            } => {
                let cond_val = self.compile_expr_recursive(cond, locals)?.into_int_value();
                let true_v = self.compile_expr_recursive(true_val, locals)?;
                let false_v = self.compile_expr_recursive(false_val, locals)?;
                Ok(self
                    .builder
                    .build_select(cond_val, true_v, false_v, "select")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            // Ret - handled at higher level
            Expr::Ret(_) => Err(CodeGenError::CodeGen(
                "ret should be handled at statement level".to_string(),
            )),

            // Memory operations
            Expr::Alloca { ty, count } => {
                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot allocate void type".to_string())
                })?;
                let alloca = if let Some(count_expr) = count {
                    let count_val = self.compile_expr_recursive(count_expr, locals)?;
                    let count_int = count_val.into_int_value();
                    self.builder
                        .build_array_alloca(llvm_ty, count_int, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                } else {
                    self.builder
                        .build_alloca(llvm_ty, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };
                Ok(alloca.into())
            }

            Expr::Load { ty, ptr } => {
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();
                let llvm_ty = self
                    .scalar_to_basic_type(ty)
                    .ok_or_else(|| CodeGenError::CodeGen("cannot load void type".to_string()))?;
                let loaded = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(loaded)
            }

            Expr::Store { value, ptr } => {
                let val = self.compile_expr_recursive(value, locals)?;
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();
                self.builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                // Store returns void, but we need a BasicValueEnum
                // Return a dummy i32 0 since void can't be a BasicValueEnum
                Ok(self.context.i32_type().const_zero().into())
            }

            // Control flow - these need block context
            Expr::Br(_) => Err(CodeGenError::CodeGen(
                "br requires block context".to_string(),
            )),
            Expr::Phi { .. } => Err(CodeGenError::CodeGen(
                "phi requires block context".to_string(),
            )),

            // Function call - needs block context for full support
            Expr::Call { name, args } => {
                // Look up the function in the module
                let function = self.module.get_function(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined function: {}", name))
                })?;

                // Compile arguments
                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in args {
                    let val = self.compile_expr_recursive(arg, locals)?;
                    compiled_args.push(val.into());
                }

                // Build the call
                let call_site = self
                    .builder
                    .build_call(function, &compiled_args, "call")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Return the result if non-void
                call_site
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodeGenError::CodeGen("call returned void".to_string()))
            }

            // For any other expressions, fall back to compile_expr
            _ => self.compile_expr(expr),
        }
    }

    /// Compile expression to LLVM value
    pub fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        // Type check first
        let checker = TypeChecker::new();
        let _ty = checker.check(expr)?;

        match expr {
            // Integer literal
            Expr::IntLit {
                ty: scalar_ty,
                value,
            } => {
                let llvm_ty = self.int_type(scalar_ty);
                // Convert i128 to u64 for LLVM (handles sign correctly)
                let val = *value as u64;
                Ok(llvm_ty
                    .const_int(val, scalar_ty != &ScalarType::I1 && *value < 0)
                    .into())
            }

            // Null pointer literal
            Expr::NullPtr => {
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // String literal - creates a global constant and returns pointer
            Expr::StringLit(s) => {
                // Create a global string constant with null terminator
                let const_array = self.context.const_string(s.as_bytes(), true);
                let array_type = const_array.get_type();

                // Create a private global for this string
                let global = self.module.add_global(array_type, None, ".str");
                global.set_initializer(&const_array);
                global.set_constant(true);

                // Return pointer to the first element
                Ok(global.as_pointer_value().into())
            }

            // Integer arithmetic - handle both scalars and vectors
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "vadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in add".to_string())),
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "vsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sub".to_string())),
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "vmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in mul".to_string())),
                }
            }
            Expr::SDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_div(l, r, "sdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_signed_div(l, r, "vsdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in sdiv".to_string())),
                }
            }
            Expr::UDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_div(l, r, "udiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_div(l, r, "vudiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in udiv".to_string())),
                }
            }
            Expr::SRem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_signed_rem(l, r, "srem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_signed_rem(l, r, "vsrem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in srem".to_string())),
                }
            }
            Expr::URem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_rem(l, r, "urem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_unsigned_rem(l, r, "vurem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in urem".to_string())),
                }
            }

            // Float literal
            Expr::FloatLit {
                ty: scalar_ty,
                value,
            } => {
                let fval = match value {
                    FloatValue::Number(n) => *n,
                    FloatValue::Inf => f64::INFINITY,
                    FloatValue::NegInf => f64::NEG_INFINITY,
                    FloatValue::Nan => f64::NAN,
                };
                match scalar_ty {
                    ScalarType::Float => Ok(self.context.f32_type().const_float(fval).into()),
                    ScalarType::Double => Ok(self.context.f64_type().const_float(fval).into()),
                    _ => Err(CodeGenError::CodeGen("invalid float type".to_string())),
                }
            }

            // Float arithmetic
            Expr::FAdd(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "fadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "vfadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fadd".to_string())),
                }
            }
            Expr::FSub(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "fsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "vfsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fsub".to_string())),
                }
            }
            Expr::FMul(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "fmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "vfmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fmul".to_string())),
                }
            }
            Expr::FDiv(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "fdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "vfdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fdiv".to_string())),
                }
            }
            Expr::FRem(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "frem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "vfrem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in frem".to_string())),
                }
            }

            // Bitwise operations
            Expr::And(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_and(lhs_val, rhs_val, "and")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::Or(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_or(lhs_val, rhs_val, "or")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::Xor(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_xor(lhs_val, rhs_val, "xor")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::Shl(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_left_shift(lhs_val, rhs_val, "shl")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::LShr(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_right_shift(lhs_val, rhs_val, false, "lshr")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::AShr(lhs, rhs) => {
                let lhs_val = self.compile_expr(lhs)?.into_int_value();
                let rhs_val = self.compile_expr(rhs)?.into_int_value();
                Ok(self
                    .builder
                    .build_right_shift(lhs_val, rhs_val, true, "ashr")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            // Integer comparison - handle both scalars and vectors
            Expr::ICmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
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
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_compare(llvm_pred, l, r, "icmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_compare(llvm_pred, l, r, "vicmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in icmp".to_string())),
                }
            }

            // Float comparison
            Expr::FCmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;
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
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_compare(llvm_pred, l, r, "fcmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_float_compare(llvm_pred, l, r, "vfcmp")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen("type mismatch in fcmp".to_string())),
                }
            }

            // Integer conversions
            Expr::Trunc {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self
                    .builder
                    .build_int_truncate(val, target, "trunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::ZExt {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self
                    .builder
                    .build_int_z_extend(val, target, "zext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::SExt {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = self.int_type(target_ty);
                Ok(self
                    .builder
                    .build_int_s_extend(val, target, "sext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            // Float conversions
            Expr::FPTrunc {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fptrunc target".to_string())),
                };
                Ok(self
                    .builder
                    .build_float_trunc(val, target, "fptrunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::FPExt {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid fpext target".to_string())),
                };
                Ok(self
                    .builder
                    .build_float_ext(val, target, "fpext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            // Float <-> Int conversions
            Expr::FPToUI {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = self.int_type(target_ty);
                Ok(self
                    .builder
                    .build_float_to_unsigned_int(val, target, "fptoui")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::FPToSI {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_float_value();
                let target = self.int_type(target_ty);
                Ok(self
                    .builder
                    .build_float_to_signed_int(val, target, "fptosi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::UIToFP {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid uitofp target".to_string())),
                };
                Ok(self
                    .builder
                    .build_unsigned_int_to_float(val, target, "uitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::SIToFP {
                ty: target_ty,
                value,
            } => {
                let val = self.compile_expr(value)?.into_int_value();
                let target = match target_ty {
                    ScalarType::Float => self.context.f32_type(),
                    ScalarType::Double => self.context.f64_type(),
                    _ => return Err(CodeGenError::CodeGen("invalid sitofp target".to_string())),
                };
                Ok(self
                    .builder
                    .build_signed_int_to_float(val, target, "sitofp")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            // Select
            Expr::Select {
                cond,
                true_val,
                false_val,
            } => {
                let cond_val = self.compile_expr(cond)?.into_int_value();
                let true_v = self.compile_expr(true_val)?;
                let false_v = self.compile_expr(false_val)?;
                Ok(self
                    .builder
                    .build_select(cond_val, true_v, false_v, "select")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            // Vector literal
            Expr::VectorLit { ty, elements } => {
                let vec_type = self.vec_type(ty);
                let mut vals: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(elements.len());
                for elem in elements {
                    vals.push(self.compile_expr(elem)?);
                }

                // Build vector from elements
                let mut vec: LLVMVectorValue<'ctx> = vec_type.get_undef();
                for (i, val) in vals.iter().enumerate() {
                    let idx = self.context.i32_type().const_int(i as u64, false);
                    vec = self
                        .builder
                        .build_insert_element(vec, *val, idx, "vec_insert")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }
                Ok(vec.into())
            }

            // Vector operations
            Expr::ExtractElement { vec, idx } => {
                let vec_val = self.compile_expr(vec)?.into_vector_value();
                let idx_val = self.compile_expr(idx)?.into_int_value();
                Ok(self
                    .builder
                    .build_extract_element(vec_val, idx_val, "extract")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            Expr::InsertElement { vec, val, idx } => {
                let vec_val = self.compile_expr(vec)?.into_vector_value();
                let elem_val = self.compile_expr(val)?;
                let idx_val = self.compile_expr(idx)?.into_int_value();
                Ok(self
                    .builder
                    .build_insert_element(vec_val, elem_val, idx_val, "insert")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            Expr::ShuffleVector { vec1, vec2, mask } => {
                let vec1_val = self.compile_expr(vec1)?.into_vector_value();
                let vec2_val = self.compile_expr(vec2)?.into_vector_value();
                let mask_val = self.compile_expr(mask)?.into_vector_value();
                Ok(self
                    .builder
                    .build_shuffle_vector(vec1_val, vec2_val, mask_val, "shuffle")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }

            // These are handled at function level, not expression level
            Expr::LocalRef(_) => Err(CodeGenError::CodeGen(
                "local references require function context".to_string(),
            )),
            Expr::Ret(_) => Err(CodeGenError::CodeGen(
                "ret requires function context".to_string(),
            )),
            Expr::Alloca { .. } => Err(CodeGenError::CodeGen(
                "alloca requires function context".to_string(),
            )),
            Expr::Load { .. } => Err(CodeGenError::CodeGen(
                "load requires function context".to_string(),
            )),
            Expr::Store { .. } => Err(CodeGenError::CodeGen(
                "store requires function context".to_string(),
            )),
            Expr::Br(_) => Err(CodeGenError::CodeGen(
                "br requires block context".to_string(),
            )),
            Expr::Phi { .. } => Err(CodeGenError::CodeGen(
                "phi requires block context".to_string(),
            )),
            Expr::Call { .. } => Err(CodeGenError::CodeGen(
                "call requires function context".to_string(),
            )),
        }
    }

    /// Create a JIT function that evaluates the expression and returns the result
    pub fn create_eval_function(&self, expr: &Expr) -> Result<()> {
        // Type check to get result type
        let checker = TypeChecker::new();
        let ty = checker.check(expr)?;

        let fn_type = match &ty {
            Type::Scalar(ScalarType::Float) => self.context.f32_type().fn_type(&[], false),
            Type::Scalar(ScalarType::Double) => self.context.f64_type().fn_type(&[], false),
            Type::Scalar(s) => {
                // All other scalars are integers
                self.int_type(s).fn_type(&[], false)
            }
            Type::Vector(_) => {
                // For vectors, we pass an output pointer as a parameter
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                self.context.void_type().fn_type(&[ptr_type.into()], false)
            }
            Type::Ptr => {
                // Pointer return type
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                ptr_type.fn_type(&[], false)
            }
        };

        let function = self.module.add_function("__lir_eval", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let result = self.compile_expr(expr)?;

        match &ty {
            Type::Vector(vt) => {
                // Store result to output pointer
                let out_ptr = function.get_first_param().unwrap().into_pointer_value();

                // For i1 vectors, we need special handling: zext each element to i8
                // because LLVM packs i1 vectors as bits, but we want them as bytes
                if vt.element == ScalarType::I1 {
                    let vec = result.into_vector_value();
                    let i8_type = self.context.i8_type();
                    for i in 0..vt.count {
                        let elem = self
                            .builder
                            .build_extract_element(
                                vec,
                                self.context.i32_type().const_int(i as u64, false),
                                "elem",
                            )
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                        let extended = self
                            .builder
                            .build_int_z_extend(elem.into_int_value(), i8_type, "zext")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                        let elem_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    i8_type,
                                    out_ptr,
                                    &[self.context.i32_type().const_int(i as u64, false)],
                                    "elemptr",
                                )
                                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        };
                        self.builder
                            .build_store(elem_ptr, extended)
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                    }
                } else {
                    self.builder
                        .build_store(out_ptr, result)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }
                self.builder
                    .build_return(None)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
            }
            _ => {
                self.builder
                    .build_return(Some(&result))
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
            }
        }

        Ok(())
    }
}

/// Typed result from JIT evaluation
#[derive(Debug, Clone)]
pub enum Value {
    I1(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Float(f32),
    Double(f64),
    Ptr(u64), // Pointer as raw address
    // Vector types - stored as Vec for flexibility
    VecI1(Vec<bool>),
    VecI8(Vec<i8>),
    VecI16(Vec<i16>),
    VecI32(Vec<i32>),
    VecI64(Vec<i64>),
    VecFloat(Vec<f32>),
    VecDouble(Vec<f64>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::I1(a), Value::I1(b)) => a == b,
            (Value::I8(a), Value::I8(b)) => a == b,
            (Value::I16(a), Value::I16(b)) => a == b,
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b || (a.is_nan() && b.is_nan()),
            (Value::Double(a), Value::Double(b)) => a == b || (a.is_nan() && b.is_nan()),
            (Value::Ptr(a), Value::Ptr(b)) => a == b,
            (Value::VecI1(a), Value::VecI1(b)) => a == b,
            (Value::VecI8(a), Value::VecI8(b)) => a == b,
            (Value::VecI16(a), Value::VecI16(b)) => a == b,
            (Value::VecI32(a), Value::VecI32(b)) => a == b,
            (Value::VecI64(a), Value::VecI64(b)) => a == b,
            (Value::VecFloat(a), Value::VecFloat(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| x == y || (x.is_nan() && y.is_nan()))
            }
            (Value::VecDouble(a), Value::VecDouble(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| x == y || (x.is_nan() && y.is_nan()))
            }
            _ => false,
        }
    }
}

/// Helper to format float with decimal point
fn format_float(v: f32) -> String {
    if v.is_nan() {
        "nan".to_string()
    } else if v.is_infinite() {
        if v > 0.0 {
            "inf".to_string()
        } else {
            "-inf".to_string()
        }
    } else {
        let s = format!("{}", v);
        if s.contains('.') || s.contains('e') {
            s
        } else {
            format!("{}.0", s)
        }
    }
}

/// Helper to format double with decimal point
fn format_double(v: f64) -> String {
    if v.is_nan() {
        "nan".to_string()
    } else if v.is_infinite() {
        if v > 0.0 {
            "inf".to_string()
        } else {
            "-inf".to_string()
        }
    } else {
        let s = format!("{}", v);
        if s.contains('.') || s.contains('e') {
            s
        } else {
            format!("{}.0", s)
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I1(v) => write!(f, "(i1 {})", if *v { 1 } else { 0 }),
            Value::I8(v) => write!(f, "(i8 {})", v),
            Value::I16(v) => write!(f, "(i16 {})", v),
            Value::I32(v) => write!(f, "(i32 {})", v),
            Value::I64(v) => write!(f, "(i64 {})", v),
            Value::Float(v) => write!(f, "(float {})", format_float(*v)),
            Value::Double(v) => write!(f, "(double {})", format_double(*v)),
            Value::Ptr(v) => {
                if *v == 0 {
                    write!(f, "(ptr null)")
                } else {
                    write!(f, "(ptr 0x{:x})", v)
                }
            }
            // Vector display: (<N x type> v1 v2 v3 ...)
            Value::VecI1(vals) => {
                write!(f, "(<{} x i1>", vals.len())?;
                for v in vals {
                    write!(f, " {}", if *v { 1 } else { 0 })?;
                }
                write!(f, ")")
            }
            Value::VecI8(vals) => {
                write!(f, "(<{} x i8>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI16(vals) => {
                write!(f, "(<{} x i16>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI32(vals) => {
                write!(f, "(<{} x i32>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI64(vals) => {
                write!(f, "(<{} x i64>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecFloat(vals) => {
                write!(f, "(<{} x float>", vals.len())?;
                for v in vals {
                    write!(f, " {}", format_float(*v))?;
                }
                write!(f, ")")
            }
            Value::VecDouble(vals) => {
                write!(f, "(<{} x double>", vals.len())?;
                for v in vals {
                    write!(f, " {}", format_double(*v))?;
                }
                write!(f, ")")
            }
        }
    }
}
