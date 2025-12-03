//! LLVM IR code generation

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, VectorType as LLVMVectorType,
};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PhiValue,
    VectorValue as LLVMVectorValue,
};

use inkwell::{AtomicOrdering, FloatPredicate, IntPredicate};
use lir_core::ast::{
    BranchTarget, Expr, ExternDecl, FCmpPred, FloatValue, FunctionDef, GepType, GlobalDef,
    ICmpPred, MemoryOrdering, ParamType, ReturnType, ScalarType, Type, VectorType,
};
use lir_core::error::TypeError;
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

/// Deferred phi incoming edges: (phi_node, [(block_label, value_expr), ...])
type DeferredPhis<'ctx> = Vec<(PhiValue<'ctx>, Vec<(String, Box<Expr>)>)>;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    /// Registry of named struct types (name -> LLVM struct type)
    struct_types: HashMap<String, inkwell::types::StructType<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            struct_types: HashMap::new(),
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

    /// Convert MemoryOrdering to LLVM AtomicOrdering
    fn atomic_ordering(ordering: &MemoryOrdering) -> AtomicOrdering {
        match ordering {
            MemoryOrdering::Monotonic => AtomicOrdering::Monotonic,
            MemoryOrdering::Acquire => AtomicOrdering::Acquire,
            MemoryOrdering::Release => AtomicOrdering::Release,
            MemoryOrdering::AcqRel => AtomicOrdering::AcquireRelease,
            MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
        }
    }

    /// Get required alignment for a value (in bytes)
    fn value_alignment(val: &BasicValueEnum<'ctx>) -> u32 {
        match val {
            BasicValueEnum::IntValue(v) => (v.get_type().get_bit_width() / 8).max(1),
            BasicValueEnum::FloatValue(_) => {
                // f32 = 4 bytes, f64 = 8 bytes
                // inkwell doesn't expose bit width directly for floats, but we can use the type
                // For now, assume 8 bytes for double, 4 for float
                8 // Default to 8 for safety
            }
            BasicValueEnum::PointerValue(_) => 8, // Assume 64-bit pointers
            BasicValueEnum::VectorValue(_) => 16, // Vectors typically need 16-byte alignment
            BasicValueEnum::ScalableVectorValue(_) => 16, // Scalable vectors
            BasicValueEnum::ArrayValue(_) => 8,
            BasicValueEnum::StructValue(_) => 8,
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

    fn param_type_to_basic_type(&self, ty: &ParamType) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            ParamType::Scalar(s) => self.scalar_to_basic_type(s),
            ParamType::Ptr => Some(
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ),
            // Ownership types compile to pointers at LLVM level
            ParamType::Own(_) | ParamType::Ref(_) | ParamType::RefMut(_) | ParamType::Rc(_) => {
                Some(
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                )
            }
        }
    }

    /// Register a named struct type
    pub fn register_struct_type(
        &mut self,
        name: &str,
        fields: &[ParamType],
    ) -> inkwell::types::StructType<'ctx> {
        let field_types: Vec<BasicTypeEnum<'ctx>> = fields
            .iter()
            .filter_map(|f| self.param_type_to_basic_type(f))
            .collect();
        let struct_type = self.context.struct_type(&field_types, false);
        self.struct_types.insert(name.to_string(), struct_type);
        struct_type
    }

    /// Get LLVM type from GepType (for getelementptr)
    fn gep_type_to_basic_type(&self, ty: &GepType) -> Result<BasicTypeEnum<'ctx>> {
        match ty {
            GepType::Scalar(s) => self
                .scalar_to_basic_type(s)
                .ok_or_else(|| CodeGenError::CodeGen("cannot GEP void type".to_string())),
            GepType::Ptr => Ok(self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            GepType::Struct(name) => self
                .struct_types
                .get(name)
                .map(|t| (*t).into())
                .ok_or_else(|| CodeGenError::CodeGen(format!("unknown struct type: {}", name))),
        }
    }

    /// Check if an index expression is a constant within bounds
    fn is_constant_in_bounds(index: &Expr, size: u32) -> bool {
        // Check if the index is a constant integer literal within bounds
        if let Expr::IntLit { value, .. } = index {
            if *value >= 0 && (*value as u32) < size {
                return true;
            }
        }
        false
    }

    /// Emit runtime bounds check for array access
    fn emit_bounds_check(&self, idx: inkwell::values::IntValue<'ctx>, size: u32) -> Result<()> {
        let current_fn = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodeGenError::CodeGen("no insert block".into()))?
            .get_parent()
            .ok_or_else(|| CodeGenError::CodeGen("block has no parent".into()))?;

        let i64_type = self.context.i64_type();
        let size_val = i64_type.const_int(size as u64, false);

        // Extend idx to i64 if needed for comparison
        let idx_i64 = if idx.get_type().get_bit_width() < 64 {
            self.builder
                .build_int_z_extend(idx, i64_type, "idx_ext")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        } else {
            idx
        };

        // Check: idx < size (unsigned comparison)
        let in_bounds = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                idx_i64,
                size_val,
                "bounds_check",
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Create blocks for bounds check
        let ok_block = self.context.append_basic_block(current_fn, "bounds_ok");
        let panic_block = self.context.append_basic_block(current_fn, "bounds_panic");

        self.builder
            .build_conditional_branch(in_bounds, ok_block, panic_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Panic block: call abort
        self.builder.position_at_end(panic_block);
        // Use libc abort() - simpler than trying to use LLVM intrinsics
        let abort_fn = self.module.get_function("abort").unwrap_or_else(|| {
            let void_type = self.context.void_type();
            let fn_type = void_type.fn_type(&[], false);
            self.module
                .add_function("abort", fn_type, Some(inkwell::module::Linkage::External))
        });
        self.builder
            .build_call(abort_fn, &[], "")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        self.builder
            .build_unreachable()
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Continue in OK block
        self.builder.position_at_end(ok_block);

        Ok(())
    }

    /// Compile a function definition
    /// Declare a function signature without compiling the body.
    /// Use this for forward declarations to enable mutual recursion.
    pub fn declare_function(&self, func: &FunctionDef) -> FunctionValue<'ctx> {
        // Check if already declared
        if let Some(existing) = self.module.get_function(&func.name) {
            return existing;
        }

        // Build parameter types
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func
            .params
            .iter()
            .filter_map(|p| self.param_type_to_basic_type(&p.ty))
            .map(|t| t.into())
            .collect();

        // Build function type
        let fn_type = match &func.return_type {
            ReturnType::Scalar(ScalarType::Void) => {
                self.context.void_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::I1) => {
                self.context.bool_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::I8) => {
                self.context.i8_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::I16) => {
                self.context.i16_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::I32) => {
                self.context.i32_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::I64) => {
                self.context.i64_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::Float) => {
                self.context.f32_type().fn_type(&param_types, false)
            }
            ReturnType::Scalar(ScalarType::Double) => {
                self.context.f64_type().fn_type(&param_types, false)
            }
            ReturnType::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(&param_types, false),
        };

        self.module.add_function(&func.name, fn_type, None)
    }

    pub fn compile_function(&self, func: &FunctionDef) -> Result<FunctionValue<'ctx>> {
        // Get or create function declaration
        let function = self.declare_function(func);

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

        // Store deferred phi incoming edges
        let mut deferred_phis: DeferredPhis<'ctx> = Vec::new();

        // Compile each block (phi nodes will be created but edges deferred)
        for block in &func.blocks {
            let bb = block_map[&block.label];
            self.builder.position_at_end(bb);

            for expr in &block.instructions {
                self.compile_expr_with_deferred_phis(
                    expr,
                    &mut locals,
                    &block_map,
                    &mut deferred_phis,
                )?;
            }
        }

        // Resolve deferred phi incoming edges
        for (phi, incoming) in &deferred_phis {
            for (label, value_expr) in incoming {
                let bb = block_map.get(label).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined block in phi: {}", label))
                })?;
                let value = self.compile_expr_recursive(value_expr, &locals)?;
                phi.add_incoming(&[(&value, *bb)]);
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
                // Ownership types compile to pointers at LLVM level
                ParamType::Own(_) | ParamType::Ref(_) | ParamType::RefMut(_) | ParamType::Rc(_) => {
                    Some(
                        self.context
                            .ptr_type(inkwell::AddressSpace::default())
                            .into(),
                    )
                }
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

    /// Compile expression with deferred phi handling
    /// Creates phi nodes but defers adding incoming edges until all locals are defined
    fn compile_expr_with_deferred_phis(
        &self,
        expr: &Expr,
        locals: &mut HashMap<String, BasicValueEnum<'ctx>>,
        blocks: &HashMap<String, inkwell::basic_block::BasicBlock<'ctx>>,
        deferred_phis: &mut DeferredPhis<'ctx>,
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

            // Phi - create the node but defer incoming edges
            Expr::Phi { ty, incoming } => {
                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot create phi for void type".to_string())
                })?;

                let phi = self
                    .builder
                    .build_phi(llvm_ty, "phi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Defer the incoming edges until all locals are defined
                deferred_phis.push((phi, incoming.clone()));

                Ok(Some(phi.as_basic_value()))
            }

            Expr::Call { name, args } => {
                let function = self.module.get_function(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined function: {}", name))
                })?;

                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in args {
                    let val = self.compile_expr_recursive(arg, locals)?;
                    compiled_args.push(val.into());
                }

                let call_site = self
                    .builder
                    .build_call(function, &compiled_args, "call")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(call_site.try_as_basic_value().basic())
            }

            Expr::TailCall { name, args } => {
                let function = self.module.get_function(name).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("undefined function: {}", name))
                })?;

                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in args {
                    let val = self.compile_expr_recursive(arg, locals)?;
                    compiled_args.push(val.into());
                }

                let call_site = self
                    .builder
                    .build_call(function, &compiled_args, "tailcall")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Mark as tail call
                call_site.set_tail_call(true);

                // Tail calls must be followed by a return
                if let Some(ret_val) = call_site.try_as_basic_value().basic() {
                    self.builder
                        .build_return(Some(&ret_val))
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }

                // TailCall is a terminator, returns None
                Ok(None)
            }

            // Array operations
            Expr::ArrayAlloc { elem_type, size } => {
                // Allocate array on stack: alloca [N x T]
                let elem_llvm_ty = self
                    .scalar_to_basic_type(elem_type)
                    .ok_or_else(|| CodeGenError::CodeGen("cannot allocate void array".into()))?;
                let array_ty = elem_llvm_ty.array_type(*size);
                let ptr = self
                    .builder
                    .build_alloca(array_ty, "array")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(Some(ptr.into()))
            }

            Expr::ArrayGet {
                elem_type,
                size,
                array,
                index,
            } => {
                let arr_ptr = self
                    .compile_expr_with_deferred_phis(array, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("array must produce a value".into()))?
                    .into_pointer_value();

                let idx_val = self
                    .compile_expr_with_deferred_phis(index, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("index must produce a value".into()))?
                    .into_int_value();

                // Bounds check (unless we can statically prove it's safe)
                let needs_check = !Self::is_constant_in_bounds(index, *size);
                if needs_check {
                    self.emit_bounds_check(idx_val, *size)?;
                }

                // GEP to get element pointer
                let i64_type = self.context.i64_type();
                let zero = i64_type.const_zero();
                let elem_llvm_ty = self
                    .scalar_to_basic_type(elem_type)
                    .ok_or_else(|| CodeGenError::CodeGen("cannot array-get void type".into()))?;
                let array_ty = elem_llvm_ty.array_type(*size);

                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(array_ty, arr_ptr, &[zero, idx_val], "elem_ptr")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };

                // Load the element
                let value = self
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, "elem")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(Some(value))
            }

            Expr::ArraySet {
                elem_type,
                size,
                array,
                index,
                value,
            } => {
                let arr_ptr = self
                    .compile_expr_with_deferred_phis(array, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("array must produce a value".into()))?
                    .into_pointer_value();

                let idx_val = self
                    .compile_expr_with_deferred_phis(index, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("index must produce a value".into()))?
                    .into_int_value();

                let val = self
                    .compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("value must produce a value".into()))?;

                // Bounds check (unless we can statically prove it's safe)
                let needs_check = !Self::is_constant_in_bounds(index, *size);
                if needs_check {
                    self.emit_bounds_check(idx_val, *size)?;
                }

                // GEP to get element pointer
                let i64_type = self.context.i64_type();
                let zero = i64_type.const_zero();
                let elem_llvm_ty = self
                    .scalar_to_basic_type(elem_type)
                    .ok_or_else(|| CodeGenError::CodeGen("cannot array-set void type".into()))?;
                let array_ty = elem_llvm_ty.array_type(*size);

                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(array_ty, arr_ptr, &[zero, idx_val], "elem_ptr")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };

                // Store the value
                self.builder
                    .build_store(elem_ptr, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Returns nothing useful (void)
                Ok(None)
            }

            Expr::ArrayLen { size, .. } => {
                // Return the compile-time constant size
                let i64_type = self.context.i64_type();
                let len = i64_type.const_int(*size as u64, false);
                Ok(Some(len.into()))
            }

            Expr::ArrayPtr { array } => {
                // Just return the array pointer as-is (for FFI)
                let arr_ptr = self
                    .compile_expr_with_deferred_phis(array, locals, blocks, deferred_phis)?
                    .ok_or_else(|| CodeGenError::CodeGen("array must produce a value".into()))?;
                Ok(Some(arr_ptr))
            }

            // Let bindings - thread through deferred_phis
            Expr::Let { bindings, body } => {
                // Evaluate each binding
                for (name, bind_expr) in bindings {
                    if let Some(value) = self.compile_expr_with_deferred_phis(
                        bind_expr,
                        locals,
                        blocks,
                        deferred_phis,
                    )? {
                        locals.insert(name.clone(), value);
                    }
                }

                // Evaluate body expressions
                let mut result = None;
                for body_expr in body {
                    result = self.compile_expr_with_deferred_phis(
                        body_expr,
                        locals,
                        blocks,
                        deferred_phis,
                    )?;
                }

                Ok(result)
            }

            // Alloca
            Expr::Alloca { ty, count } => {
                let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid alloca type: {:?}", ty))
                })?;

                let ptr = if let Some(count_expr) = count {
                    let count_val = self
                        .compile_expr_with_deferred_phis(count_expr, locals, blocks, deferred_phis)?
                        .ok_or_else(|| CodeGenError::CodeGen("count must produce a value".into()))?
                        .into_int_value();
                    self.builder
                        .build_array_alloca(llvm_ty, count_val, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                } else {
                    self.builder
                        .build_alloca(llvm_ty, "alloca")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                };

                Ok(Some(ptr.into()))
            }

            // Load
            Expr::Load { ty, ptr } => {
                let ptr_val = self
                    .compile_expr_with_deferred_phis(ptr, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("load pointer must produce a value".into())
                    })?
                    .into_pointer_value();

                let llvm_ty = self
                    .param_type_to_basic_type(ty)
                    .ok_or_else(|| CodeGenError::CodeGen(format!("invalid load type: {:?}", ty)))?;

                let val = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(Some(val))
            }

            // Store
            Expr::Store { value, ptr } => {
                let val = self
                    .compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("store value must produce a value".into())
                    })?;
                let ptr_val = self
                    .compile_expr_with_deferred_phis(ptr, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("store pointer must produce a value".into())
                    })?
                    .into_pointer_value();

                self.builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                Ok(None)
            }

            // Atomic Load
            Expr::AtomicLoad { ordering, ty, ptr } => {
                let ptr_val = self
                    .compile_expr_with_deferred_phis(ptr, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("atomic load pointer must produce a value".into())
                    })?
                    .into_pointer_value();

                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid atomic load type: {:?}", ty))
                })?;

                let load = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "atomic_load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the load instruction
                let inst = load.as_instruction_value().ok_or_else(|| {
                    CodeGenError::CodeGen("failed to get instruction from load".into())
                })?;
                inst.set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                // Atomic operations require proper alignment (at least size of the type)
                let align = (ty.bit_width() / 8).max(1);
                inst.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(Some(load))
            }

            // Atomic Store
            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => {
                let val = self
                    .compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("atomic store value must produce a value".into())
                    })?;
                let ptr_val = self
                    .compile_expr_with_deferred_phis(ptr, locals, blocks, deferred_phis)?
                    .ok_or_else(|| {
                        CodeGenError::CodeGen("atomic store pointer must produce a value".into())
                    })?
                    .into_pointer_value();

                let store = self
                    .builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the store instruction
                store
                    .set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                // Get the value type's bit width for alignment
                let align = Self::value_alignment(&val);
                store.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(None)
            }

            // Ownership operations
            Expr::AllocOwn { elem_type } => {
                // Allocate space for the owned value on the stack
                let llvm_ty = self.scalar_to_basic_type(elem_type).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot allocate void type".to_string())
                })?;
                let ptr = self
                    .builder
                    .build_alloca(llvm_ty, "own")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(Some(ptr.into()))
            }

            Expr::BorrowRef { value } => {
                // Borrow creates a reference - at runtime this is just the pointer
                self.compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)
            }

            Expr::BorrowRefMut { value } => {
                // Mutable borrow - at runtime this is just the pointer
                self.compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)
            }

            Expr::Drop { value } => {
                // Drop - for now this is a no-op. In the future with RC, it would decrement
                // the reference count and potentially deallocate
                let _ =
                    self.compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)?;
                // Drop returns None (void)
                Ok(None)
            }

            Expr::Move { value } => {
                // Move - at runtime this is just returning the pointer
                // The borrow checker ensures the source is invalidated
                self.compile_expr_with_deferred_phis(value, locals, blocks, deferred_phis)
            }

            // Ret
            Expr::Ret(maybe_val) => {
                match maybe_val {
                    Some(val_expr) => {
                        let val = self
                            .compile_expr_with_deferred_phis(
                                val_expr,
                                locals,
                                blocks,
                                deferred_phis,
                            )?
                            .ok_or_else(|| {
                                CodeGenError::CodeGen("ret value must produce a value".into())
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
                Ok(None)
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
                let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
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
                    .param_type_to_basic_type(ty)
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

            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                let ptr_val = self.compile_expr_recursive(ptr, locals)?;
                let ptr_val = ptr_val.into_pointer_value();

                // Get the element type
                let elem_ty = self.gep_type_to_basic_type(ty)?;

                // Compile indices
                let compiled_indices: Vec<inkwell::values::IntValue> = indices
                    .iter()
                    .map(|idx| {
                        self.compile_expr_recursive(idx, locals)
                            .map(|v| v.into_int_value())
                    })
                    .collect::<Result<Vec<_>>>()?;

                // Build GEP
                let gep = if *inbounds {
                    unsafe {
                        self.builder
                            .build_in_bounds_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                } else {
                    unsafe {
                        self.builder
                            .build_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                };

                Ok(gep.into())
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

            // Tail call - needs block context for proper use
            Expr::TailCall { .. } => Err(CodeGenError::CodeGen(
                "tailcall requires block context".to_string(),
            )),

            // Array operations - need block context
            Expr::ArrayAlloc { .. }
            | Expr::ArrayGet { .. }
            | Expr::ArraySet { .. }
            | Expr::ArrayLen { .. }
            | Expr::ArrayPtr { .. } => Err(CodeGenError::CodeGen(
                "array operations require block context".to_string(),
            )),

            // Conversions - need locals for the value expression
            Expr::Trunc { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_truncate(val, target, "trunc")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::ZExt { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_z_extend(val, target, "zext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::SExt { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_int_s_extend(val, target, "sext")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::FPTrunc { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = match ty {
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
            Expr::FPExt { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = match ty {
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
            Expr::FPToUI { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_float_to_unsigned_int(val, target, "fptoui")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::FPToSI { ty, value } => {
                let val = self
                    .compile_expr_recursive(value, locals)?
                    .into_float_value();
                let target = self.int_type(ty);
                Ok(self
                    .builder
                    .build_float_to_signed_int(val, target, "fptosi")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    .into())
            }
            Expr::UIToFP { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = match ty {
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
            Expr::SIToFP { ty, value } => {
                let val = self.compile_expr_recursive(value, locals)?.into_int_value();
                let target = match ty {
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

            // Ownership operations with locals context
            Expr::AllocOwn { elem_type } => {
                let llvm_ty = self.scalar_to_basic_type(elem_type).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot allocate void type".to_string())
                })?;
                let ptr = self
                    .builder
                    .build_alloca(llvm_ty, "own")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(ptr.into())
            }

            Expr::BorrowRef { value } => self.compile_expr_recursive(value, locals),

            Expr::BorrowRefMut { value } => self.compile_expr_recursive(value, locals),

            Expr::Drop { value } => {
                let _ = self.compile_expr_recursive(value, locals)?;
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            Expr::Move { value } => self.compile_expr_recursive(value, locals),

            // Reference counting operations with locals context
            Expr::RcAlloc { elem_type } => self.compile_rc_alloc(elem_type),
            Expr::RcClone { value } => {
                let data_ptr = self
                    .compile_expr_recursive(value, locals)?
                    .into_pointer_value();
                self.compile_rc_clone_ptr(data_ptr)
            }
            Expr::RcDrop { value } => {
                let data_ptr = self
                    .compile_expr_recursive(value, locals)?
                    .into_pointer_value();
                self.compile_rc_drop_ptr(data_ptr)
            }
            Expr::RcCount { value } => {
                let data_ptr = self
                    .compile_expr_recursive(value, locals)?
                    .into_pointer_value();
                self.compile_rc_count_ptr(data_ptr)
            }
            Expr::RcPtr { value } => self.compile_expr_recursive(value, locals),

            // Atomic operations with locals context
            Expr::AtomicLoad { ordering, ty, ptr } => {
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();

                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid atomic load type: {:?}", ty))
                })?;

                let load = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "atomic_load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the load instruction
                let inst = load.as_instruction_value().ok_or_else(|| {
                    CodeGenError::CodeGen("failed to get instruction from load".into())
                })?;
                inst.set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = (ty.bit_width() / 8).max(1);
                inst.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(load)
            }
            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => {
                let val = self.compile_expr_recursive(value, locals)?;
                let ptr_val = self
                    .compile_expr_recursive(ptr, locals)?
                    .into_pointer_value();

                let store = self
                    .builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the store instruction
                store
                    .set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = Self::value_alignment(&val);
                store.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                // Return null pointer as void indicator
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // For any other expressions, fall back to compile_expr
            _ => self.compile_expr(expr),
        }
    }

    /// Compile expression to LLVM value
    /// Note: Type checking should be done at a higher level (JIT's eval/eval_function)
    /// before calling this. This allows function bodies to compile correctly since
    /// the type checker needs function context for parameters.
    pub fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
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
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                let ptr_val = self.compile_expr(ptr)?.into_pointer_value();

                // Get the element type
                let elem_ty = self.gep_type_to_basic_type(ty)?;

                // Compile indices
                let compiled_indices: Vec<inkwell::values::IntValue> = indices
                    .iter()
                    .map(|idx| self.compile_expr(idx).map(|v| v.into_int_value()))
                    .collect::<Result<Vec<_>>>()?;

                // Build GEP
                let gep = if *inbounds {
                    unsafe {
                        self.builder
                            .build_in_bounds_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                } else {
                    unsafe {
                        self.builder
                            .build_gep(elem_ty, ptr_val, &compiled_indices, "gep")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                    }
                };

                Ok(gep.into())
            }
            Expr::Br(_) => Err(CodeGenError::CodeGen(
                "br requires block context".to_string(),
            )),
            Expr::Phi { .. } => Err(CodeGenError::CodeGen(
                "phi requires block context".to_string(),
            )),
            Expr::Call { .. } => Err(CodeGenError::CodeGen(
                "call requires function context".to_string(),
            )),

            Expr::TailCall { .. } => Err(CodeGenError::CodeGen(
                "tailcall requires block context".to_string(),
            )),

            // Array operations - need block context
            Expr::ArrayAlloc { .. }
            | Expr::ArrayGet { .. }
            | Expr::ArraySet { .. }
            | Expr::ArrayLen { .. }
            | Expr::ArrayPtr { .. } => Err(CodeGenError::CodeGen(
                "array operations require block context".to_string(),
            )),

            // Struct literal: { val1 val2 ... }
            Expr::StructLit(fields) => {
                // Compile all field values
                let mut compiled_fields: Vec<BasicValueEnum<'ctx>> = Vec::new();
                for field in fields {
                    compiled_fields.push(self.compile_expr(field)?);
                }

                // Build a struct type based on the field types
                let field_types: Vec<BasicTypeEnum<'ctx>> =
                    compiled_fields.iter().map(|v| v.get_type()).collect();
                let struct_type = self.context.struct_type(&field_types, false);

                // Build struct value
                let mut struct_val = struct_type.get_undef();
                for (i, val) in compiled_fields.iter().enumerate() {
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, *val, i as u32, "struct_field")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into_struct_value();
                }
                Ok(struct_val.into())
            }

            // Extract a value from a struct
            Expr::ExtractValue { aggregate, indices } => {
                let agg_val = self.compile_expr(aggregate)?.into_struct_value();
                // For now, we only support single-level indices
                let idx = indices.first().ok_or_else(|| {
                    CodeGenError::CodeGen("extractvalue requires at least one index".to_string())
                })?;
                let result = self
                    .builder
                    .build_extract_value(agg_val, *idx, "extractvalue")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(result)
            }

            // Insert a value into a struct
            Expr::InsertValue {
                aggregate,
                value,
                indices,
            } => {
                let agg_val = self.compile_expr(aggregate)?.into_struct_value();
                let val = self.compile_expr(value)?;
                // For now, we only support single-level indices
                let idx = indices.first().ok_or_else(|| {
                    CodeGenError::CodeGen("insertvalue requires at least one index".to_string())
                })?;
                let result = self
                    .builder
                    .build_insert_value(agg_val, val, *idx, "insertvalue")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                // Convert AggregateValueEnum to BasicValueEnum
                Ok(result.into_struct_value().into())
            }

            // Ownership operations - these compile to simple pointer operations
            // The ownership semantics are verified by a separate borrow-check pass
            Expr::AllocOwn { elem_type } => {
                // Allocate space for the owned value on the stack
                let llvm_ty = self.scalar_to_basic_type(elem_type).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot allocate void type".to_string())
                })?;
                let ptr = self
                    .builder
                    .build_alloca(llvm_ty, "own")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                Ok(ptr.into())
            }

            Expr::BorrowRef { value } => {
                // Borrow creates a reference - at runtime this is just the pointer
                self.compile_expr(value)
            }

            Expr::BorrowRefMut { value } => {
                // Mutable borrow - at runtime this is just the pointer
                self.compile_expr(value)
            }

            Expr::Drop { value } => {
                // Drop - for now this is a no-op. In the future with RC, it would decrement
                // the reference count and potentially deallocate
                let _ = self.compile_expr(value)?;
                // Return void-equivalent (null pointer as placeholder)
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            Expr::Move { value } => {
                // Move - at runtime this is just returning the pointer
                // The borrow checker ensures the source is invalidated
                self.compile_expr(value)
            }

            // Reference counting operations
            Expr::RcAlloc { elem_type } => self.compile_rc_alloc(elem_type),
            Expr::RcClone { value } => self.compile_rc_clone(value),
            Expr::RcDrop { value } => self.compile_rc_drop(value),
            Expr::RcCount { value } => self.compile_rc_count(value),
            Expr::RcPtr { value } => {
                // rc-ptr just returns the data pointer (which is what we store)
                self.compile_expr(value)
            }

            // Atomic operations
            Expr::AtomicLoad { ordering, ty, ptr } => {
                let ptr_val = self.compile_expr(ptr)?.into_pointer_value();

                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid atomic load type: {:?}", ty))
                })?;

                let load = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "atomic_load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the load instruction
                let inst = load.as_instruction_value().ok_or_else(|| {
                    CodeGenError::CodeGen("failed to get instruction from load".into())
                })?;
                inst.set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = (ty.bit_width() / 8).max(1);
                inst.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(load)
            }
            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => {
                let val = self.compile_expr(value)?;
                let ptr_val = self.compile_expr(ptr)?.into_pointer_value();

                let store = self
                    .builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the store instruction
                store
                    .set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = Self::value_alignment(&val);
                store.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                // Return null pointer as void indicator
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // Let bindings - use the recursive helper with empty initial locals
            Expr::Let { bindings, body } => {
                let locals: HashMap<String, BasicValueEnum<'ctx>> = HashMap::new();
                self.compile_let_expr(bindings, body, &locals)
            }
        }
    }

    /// Compile rc-alloc: allocate with refcount header
    fn compile_rc_alloc(&self, elem_type: &ScalarType) -> Result<BasicValueEnum<'ctx>> {
        // Get or declare malloc
        let malloc_fn = self.get_or_declare_malloc()?;

        // Calculate size: sizeof(i64) + sizeof(T)
        let elem_size = elem_type.bit_width() / 8;
        let total_size = 8 + elem_size as u64; // refcount (i64) + data

        let i64_type = self.context.i64_type();
        let size_val = i64_type.const_int(total_size, false);

        // Call malloc
        let call_site = self
            .builder
            .build_call(malloc_fn, &[size_val.into()], "rc_base")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let base_ptr = call_site
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodeGenError::CodeGen("malloc returned void".to_string()))?
            .into_pointer_value();

        // Initialize refcount to 1
        let one = i64_type.const_int(1, false);
        self.builder
            .build_store(base_ptr, one)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return pointer to data (8 bytes after base)
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let i8_type = self.context.i8_type();
        let eight = i64_type.const_int(8, false);
        let data_ptr = unsafe {
            self.builder
                .build_gep(i8_type, base_ptr, &[eight], "rc_data")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };
        let data_ptr = self
            .builder
            .build_pointer_cast(data_ptr, ptr_type, "rc_ptr")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(data_ptr.into())
    }

    /// Compile rc-clone: increment refcount atomically
    fn compile_rc_clone(&self, value: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self.compile_expr(value)?.into_pointer_value();

        // Get refcount field (8 bytes before data)
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        // Atomic increment
        let one = i64_type.const_int(1, false);
        self.builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Add,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Return the same data pointer
        Ok(data_ptr.into())
    }

    /// Compile rc-drop: decrement refcount, free if zero
    fn compile_rc_drop(&self, value: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self.compile_expr(value)?.into_pointer_value();

        // Get refcount field (8 bytes before data)
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        // Atomic decrement, get old value
        let one = i64_type.const_int(1, false);
        let old_rc = self
            .builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Sub,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Check if old was 1 (now 0)
        let was_one = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, old_rc, one, "was_one")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Create blocks for conditional free
        let current_fn = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodeGenError::CodeGen("no insert block".to_string()))?
            .get_parent()
            .ok_or_else(|| CodeGenError::CodeGen("no parent function".to_string()))?;

        let free_block = self.context.append_basic_block(current_fn, "rc_free");
        let cont_block = self.context.append_basic_block(current_fn, "rc_cont");

        self.builder
            .build_conditional_branch(was_one, free_block, cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Free block: call free on base pointer
        self.builder.position_at_end(free_block);
        let free_fn = self.get_or_declare_free()?;
        // Base pointer is rc_ptr (we already have it pointing at refcount)
        self.builder
            .build_call(free_fn, &[rc_ptr.into()], "")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        // Continue block
        self.builder.position_at_end(cont_block);

        // Return void (represented as null pointer for now)
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        Ok(ptr_type.const_null().into())
    }

    /// Compile rc-count: read refcount
    fn compile_rc_count(&self, value: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let data_ptr = self.compile_expr(value)?.into_pointer_value();

        // Get refcount field (8 bytes before data)
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        // Load the refcount
        let count = self
            .builder
            .build_load(i64_type, rc_ptr, "rc_count")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(count)
    }

    /// Get or declare malloc function
    fn get_or_declare_malloc(&self) -> Result<FunctionValue<'ctx>> {
        if let Some(fn_val) = self.module.get_function("malloc") {
            return Ok(fn_val);
        }

        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
        Ok(self.module.add_function("malloc", fn_type, None))
    }

    /// Get or declare free function
    fn get_or_declare_free(&self) -> Result<FunctionValue<'ctx>> {
        if let Some(fn_val) = self.module.get_function("free") {
            return Ok(fn_val);
        }

        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        Ok(self.module.add_function("free", fn_type, None))
    }

    /// Helper for let bindings - compile with a locals map
    fn compile_let_expr(
        &self,
        bindings: &[(String, Box<Expr>)],
        body: &[Expr],
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let mut new_locals = locals.clone();

        // Evaluate each binding and add to locals
        for (name, expr) in bindings {
            let value = self.compile_with_locals(expr, &new_locals)?;
            new_locals.insert(name.clone(), value);
        }

        // Evaluate body expressions, return last
        let mut result = None;
        for expr in body {
            result = Some(self.compile_with_locals(expr, &new_locals)?);
        }

        result.ok_or_else(|| CodeGenError::CodeGen("empty let body".to_string()))
    }

    /// Compile expression with locals context (for let bindings)
    fn compile_with_locals(
        &self,
        expr: &Expr,
        locals: &HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::LocalRef(name) => locals
                .get(name)
                .copied()
                .ok_or_else(|| CodeGenError::CodeGen(format!("undefined variable: {}", name))),

            Expr::Let { bindings, body } => self.compile_let_expr(bindings, body, locals),

            // For binary operations, we need to recurse with locals
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    (BasicValueEnum::VectorValue(l), BasicValueEnum::VectorValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "add requires integer operands".to_string(),
                    )),
                }
            }

            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "sub requires integer operands".to_string(),
                    )),
                }
            }

            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "mul requires integer operands".to_string(),
                    )),
                }
            }

            // Float arithmetic operations
            Expr::FAdd(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_add(l, r, "fadd")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "fadd requires float operands".to_string(),
                    )),
                }
            }

            Expr::FSub(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_sub(l, r, "fsub")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "fsub requires float operands".to_string(),
                    )),
                }
            }

            Expr::FMul(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_mul(l, r, "fmul")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "fmul requires float operands".to_string(),
                    )),
                }
            }

            Expr::FDiv(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_div(l, r, "fdiv")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "fdiv requires float operands".to_string(),
                    )),
                }
            }

            Expr::FRem(lhs, rhs) => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                        .builder
                        .build_float_rem(l, r, "frem")
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
                        .into()),
                    _ => Err(CodeGenError::CodeGen(
                        "frem requires float operands".to_string(),
                    )),
                }
            }

            // Integer comparison
            Expr::ICmp { pred, lhs, rhs } => {
                let lhs_val = self.compile_with_locals(lhs, locals)?;
                let rhs_val = self.compile_with_locals(rhs, locals)?;
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
                    _ => Err(CodeGenError::CodeGen(
                        "icmp requires integer operands".to_string(),
                    )),
                }
            }

            // Select (ternary conditional)
            Expr::Select {
                cond,
                true_val,
                false_val,
            } => {
                let cond_val = self.compile_with_locals(cond, locals)?.into_int_value();
                let true_val = self.compile_with_locals(true_val, locals)?;
                let false_val = self.compile_with_locals(false_val, locals)?;
                Ok(self
                    .builder
                    .build_select(cond_val, true_val, false_val, "select")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            // Reference counting operations with locals support
            Expr::RcAlloc { elem_type } => self.compile_rc_alloc(elem_type),
            Expr::RcClone { value } => {
                let data_ptr = self
                    .compile_with_locals(value, locals)?
                    .into_pointer_value();
                self.compile_rc_clone_ptr(data_ptr)
            }
            Expr::RcDrop { value } => {
                let data_ptr = self
                    .compile_with_locals(value, locals)?
                    .into_pointer_value();
                self.compile_rc_drop_ptr(data_ptr)
            }
            Expr::RcCount { value } => {
                let data_ptr = self
                    .compile_with_locals(value, locals)?
                    .into_pointer_value();
                self.compile_rc_count_ptr(data_ptr)
            }
            Expr::RcPtr { value } => self.compile_with_locals(value, locals),

            // Store with locals support
            Expr::Store { value, ptr } => {
                let ptr_val = self.compile_with_locals(ptr, locals)?.into_pointer_value();
                let val = self.compile_with_locals(value, locals)?;
                self.builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // Load with locals support
            Expr::Load { ty, ptr } => {
                let ptr_val = self.compile_with_locals(ptr, locals)?.into_pointer_value();
                let llvm_type = self
                    .param_type_to_basic_type(ty)
                    .ok_or_else(|| CodeGenError::CodeGen("void type in load".to_string()))?;
                Ok(self
                    .builder
                    .build_load(llvm_type, ptr_val, "load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?)
            }

            // Atomic operations with locals support
            Expr::AtomicLoad { ordering, ty, ptr } => {
                let ptr_val = self.compile_with_locals(ptr, locals)?.into_pointer_value();

                let llvm_ty = self.scalar_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen(format!("invalid atomic load type: {:?}", ty))
                })?;

                let load = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "atomic_load")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the load instruction
                let inst = load.as_instruction_value().ok_or_else(|| {
                    CodeGenError::CodeGen("failed to get instruction from load".into())
                })?;
                inst.set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = (ty.bit_width() / 8).max(1);
                inst.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                Ok(load)
            }
            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => {
                let val = self.compile_with_locals(value, locals)?;
                let ptr_val = self.compile_with_locals(ptr, locals)?.into_pointer_value();

                let store = self
                    .builder
                    .build_store(ptr_val, val)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Set atomic ordering and alignment on the store instruction
                store
                    .set_atomic_ordering(Self::atomic_ordering(ordering))
                    .map_err(|e| {
                        CodeGenError::CodeGen(format!("failed to set atomic ordering: {}", e))
                    })?;
                let align = Self::value_alignment(&val);
                store.set_alignment(align).map_err(|e| {
                    CodeGenError::CodeGen(format!("failed to set alignment: {}", e))
                })?;

                // Return null pointer as void indicator
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                Ok(ptr_type.const_null().into())
            }

            // For simple literals and non-nested expressions, delegate to compile_expr
            _ => self.compile_expr(expr),
        }
    }

    /// Helper for rc-clone with pre-computed pointer
    fn compile_rc_clone_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let one = i64_type.const_int(1, false);
        self.builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Add,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(data_ptr.into())
    }

    /// Helper for rc-drop with pre-computed pointer
    fn compile_rc_drop_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let one = i64_type.const_int(1, false);
        let old_rc = self
            .builder
            .build_atomicrmw(
                inkwell::AtomicRMWBinOp::Sub,
                rc_ptr,
                one,
                inkwell::AtomicOrdering::SequentiallyConsistent,
            )
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let was_one = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, old_rc, one, "was_one")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        let current_fn = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodeGenError::CodeGen("no insert block".to_string()))?
            .get_parent()
            .ok_or_else(|| CodeGenError::CodeGen("no parent function".to_string()))?;

        let free_block = self.context.append_basic_block(current_fn, "rc_free");
        let cont_block = self.context.append_basic_block(current_fn, "rc_cont");

        self.builder
            .build_conditional_branch(was_one, free_block, cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        self.builder.position_at_end(free_block);
        let free_fn = self.get_or_declare_free()?;
        self.builder
            .build_call(free_fn, &[rc_ptr.into()], "")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        self.builder.position_at_end(cont_block);

        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        Ok(ptr_type.const_null().into())
    }

    /// Helper for rc-count with pre-computed pointer
    fn compile_rc_count_ptr(
        &self,
        data_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let neg_eight = i64_type.const_int((-8i64) as u64, true);
        let rc_ptr = unsafe {
            self.builder
                .build_gep(i8_type, data_ptr, &[neg_eight], "rc_field")
                .map_err(|e| CodeGenError::CodeGen(e.to_string()))?
        };

        let count = self
            .builder
            .build_load(i64_type, rc_ptr, "rc_count")
            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

        Ok(count)
    }

    /// Create a JIT function that evaluates the expression and returns the result
    /// The type should be pre-computed by the caller via type checking.
    pub fn create_eval_function(&self, expr: &Expr, ty: &Type) -> Result<()> {
        let fn_type = match ty {
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
    // Struct type - stored as Vec of boxed Values
    Struct(Vec<Value>),
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
            (Value::Struct(a), Value::Struct(b)) => a == b,
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
            // Struct display: { val1 val2 ... }
            Value::Struct(fields) => {
                write!(f, "{{")?;
                for (i, val) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, " {}", val)?;
                }
                write!(f, " }}")
            }
        }
    }
}
