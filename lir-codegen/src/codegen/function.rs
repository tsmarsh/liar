//! Function compilation for LLVM codegen
//!
//! Handles function declarations, definitions, extern decls, and globals.

use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use lir_core::ast::{
    BranchTarget, Expr, ExternDecl, FloatValue, FunctionDef, GlobalDef, ParamType, ScalarType,
};
use std::collections::HashMap;

use super::{CodeGenError, DeferredPhis, Result};

/// Function compilation methods for CodeGen
impl<'ctx> super::CodeGen<'ctx> {
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
        let fn_type = self.build_return_type(&func.return_type, &param_types, false);

        self.module.add_function(&func.name, fn_type, None)
    }

    /// Compile a function definition
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
        let llvm_param_count = function.count_params();
        let lir_param_count = func.params.len() as u32;
        if llvm_param_count != lir_param_count {
            return Err(CodeGenError::CodeGen(format!(
                "Function '{}' param count mismatch: LLVM has {} params, lIR has {}. Params: {:?}",
                func.name,
                llvm_param_count,
                lir_param_count,
                func.params
                    .iter()
                    .map(|p| format!("{}: {:?}", p.name, p.ty))
                    .collect::<Vec<_>>()
            )));
        }
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
                // Anonymous struct types (for closures)
                ParamType::AnonStruct(fields) => {
                    let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                        .iter()
                        .filter_map(|f| self.param_type_to_basic_type(f))
                        .collect();
                    Some(self.context.struct_type(&field_types, false).into())
                }
            })
            .map(|t| t.into())
            .collect();

        // Build function type
        let fn_type = self.build_return_type(&decl.return_type, &param_types, decl.varargs);

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
    pub(crate) fn compile_expr_with_deferred_phis(
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
                let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
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

                let call = self
                    .builder
                    .build_call(function, &compiled_args, "call")
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Try to get return value (may be void)
                Ok(call.try_as_basic_value().basic())
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

                // Mark as tail call for optimization
                call_site.set_tail_call(true);

                // Tail call must be immediately followed by ret
                if let Some(ret_val) = call_site.try_as_basic_value().basic() {
                    self.builder
                        .build_return(Some(&ret_val))
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }

                Ok(None)
            }

            Expr::IndirectTailCall {
                fn_ptr,
                ret_ty,
                args,
            } => {
                // Compile the function pointer
                let fn_ptr_val = self.compile_expr_recursive(fn_ptr, locals)?;

                // Build the function type from ret_ty and arg types
                let ret_llvm_ty = self.param_type_to_basic_type(ret_ty);
                let arg_types: Vec<inkwell::types::BasicMetadataTypeEnum> = args
                    .iter()
                    .map(|_| {
                        self.context
                            .ptr_type(inkwell::AddressSpace::default())
                            .into()
                    })
                    .collect();
                let fn_type = match ret_llvm_ty {
                    Some(basic_ty) => basic_ty.fn_type(&arg_types, false),
                    None => self.context.void_type().fn_type(&arg_types, false),
                };

                // Compile the arguments
                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in args {
                    let val = self.compile_expr_recursive(arg, locals)?;
                    compiled_args.push(val.into());
                }

                // Build indirect call and mark as tail call
                let call_site = self
                    .builder
                    .build_indirect_call(
                        fn_type,
                        fn_ptr_val.into_pointer_value(),
                        &compiled_args,
                        "indirect_tailcall",
                    )
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;

                // Mark as tail call for optimization
                call_site.set_tail_call(true);

                // Tail call must be immediately followed by ret
                if let Some(ret_val) = call_site.try_as_basic_value().basic() {
                    self.builder
                        .build_return(Some(&ret_val))
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }

                Ok(None)
            }

            Expr::Ret(val) => {
                if let Some(val) = val {
                    // Handle phi specially in return position
                    let ret_val = if let Expr::Phi { ty, incoming } = val.as_ref() {
                        let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
                            CodeGenError::CodeGen("cannot create phi for void type".to_string())
                        })?;
                        let phi = self
                            .builder
                            .build_phi(llvm_ty, "ret_phi")
                            .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                        deferred_phis.push((phi, incoming.clone()));
                        phi.as_basic_value()
                    } else {
                        self.compile_expr_recursive(val, locals)?
                    };
                    self.builder
                        .build_return(Some(&ret_val))
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                }
                Ok(None)
            }

            // Let binding in function context
            Expr::Let { bindings, body } => {
                let result = self.compile_let_expr_with_deferred_phis(
                    bindings,
                    body,
                    locals,
                    blocks,
                    deferred_phis,
                )?;
                Ok(Some(result))
            }

            // Regular expression - compile and optionally bind
            _ => {
                let result = self.compile_expr_recursive(expr, locals)?;
                Ok(Some(result))
            }
        }
    }

    /// Compile a let expression with deferred phi handling
    pub(crate) fn compile_let_expr_with_deferred_phis(
        &self,
        bindings: &[(String, Box<Expr>)],
        body: &[Expr],
        locals: &mut HashMap<String, BasicValueEnum<'ctx>>,
        blocks: &HashMap<String, inkwell::basic_block::BasicBlock<'ctx>>,
        deferred_phis: &mut DeferredPhis<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        // Compile bindings - phi nodes need special handling
        for (name, init) in bindings {
            let val = if let Expr::Phi { ty, incoming } = init.as_ref() {
                // Handle phi specially - create node now, defer incoming edges
                let llvm_ty = self.param_type_to_basic_type(ty).ok_or_else(|| {
                    CodeGenError::CodeGen("cannot create phi for void type".to_string())
                })?;
                let phi = self
                    .builder
                    .build_phi(llvm_ty, name)
                    .map_err(|e| CodeGenError::CodeGen(e.to_string()))?;
                deferred_phis.push((phi, incoming.clone()));
                phi.as_basic_value()
            } else {
                self.compile_expr_recursive(init, locals)?
            };
            locals.insert(name.clone(), val);
        }

        // Compile body expressions
        let mut result: BasicValueEnum<'ctx> = self.context.i64_type().const_int(0, false).into();
        for expr in body {
            if let Some(val) =
                self.compile_expr_with_deferred_phis(expr, locals, blocks, deferred_phis)?
            {
                result = val;
            }
        }

        Ok(result)
    }
}
