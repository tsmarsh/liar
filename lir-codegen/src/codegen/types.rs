//! Type conversion utilities for LLVM codegen
//!
//! Converts lIR types to LLVM types.

use inkwell::types::{BasicTypeEnum, IntType, VectorType as LLVMVectorType};
use inkwell::{AtomicOrdering, AtomicRMWBinOp};
use lir_core::ast::{
    AtomicRMWOp, GepType, MemoryOrdering, ParamType, ReturnType, ScalarType, Type, VectorType,
};

use super::{CodeGenError, Result};

/// Type conversion methods for CodeGen
impl<'ctx> super::CodeGen<'ctx> {
    /// Get LLVM integer type from ScalarType
    pub(crate) fn int_type(&self, ty: &ScalarType) -> IntType<'ctx> {
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
    pub(crate) fn atomic_ordering(ordering: &MemoryOrdering) -> AtomicOrdering {
        match ordering {
            MemoryOrdering::Monotonic => AtomicOrdering::Monotonic,
            MemoryOrdering::Acquire => AtomicOrdering::Acquire,
            MemoryOrdering::Release => AtomicOrdering::Release,
            MemoryOrdering::AcqRel => AtomicOrdering::AcquireRelease,
            MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
        }
    }

    /// Convert MemoryOrdering to LLVM failure ordering for cmpxchg
    /// Per LLVM spec: failure ordering cannot be Release or AcqRel
    pub(crate) fn failure_ordering(ordering: &MemoryOrdering) -> AtomicOrdering {
        match ordering {
            MemoryOrdering::Monotonic => AtomicOrdering::Monotonic,
            MemoryOrdering::Acquire => AtomicOrdering::Acquire,
            // Release/AcqRel not valid for failure - use Monotonic/Acquire respectively
            MemoryOrdering::Release => AtomicOrdering::Monotonic,
            MemoryOrdering::AcqRel => AtomicOrdering::Acquire,
            MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
        }
    }

    /// Convert AtomicRMWOp to LLVM AtomicRMWBinOp
    pub(crate) fn atomic_rmw_op(op: &AtomicRMWOp) -> AtomicRMWBinOp {
        match op {
            AtomicRMWOp::Xchg => AtomicRMWBinOp::Xchg,
            AtomicRMWOp::Add => AtomicRMWBinOp::Add,
            AtomicRMWOp::Sub => AtomicRMWBinOp::Sub,
            AtomicRMWOp::And => AtomicRMWBinOp::And,
            AtomicRMWOp::Or => AtomicRMWBinOp::Or,
            AtomicRMWOp::Xor => AtomicRMWBinOp::Xor,
            AtomicRMWOp::Min => AtomicRMWBinOp::Min,
            AtomicRMWOp::Max => AtomicRMWBinOp::Max,
            AtomicRMWOp::UMin => AtomicRMWBinOp::UMin,
            AtomicRMWOp::UMax => AtomicRMWBinOp::UMax,
        }
    }

    /// Get LLVM vector type
    pub(crate) fn vec_type(&self, ty: &VectorType) -> LLVMVectorType<'ctx> {
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
    pub(crate) fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
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
    pub(crate) fn scalar_to_basic_type(&self, ty: &ScalarType) -> Option<BasicTypeEnum<'ctx>> {
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

    /// Get LLVM type from ParamType
    pub(crate) fn param_type_to_basic_type(&self, ty: &ParamType) -> Option<BasicTypeEnum<'ctx>> {
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
            ParamType::AnonStruct(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .filter_map(|f| self.param_type_to_basic_type(f))
                    .collect();
                Some(self.context.struct_type(&field_types, false).into())
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
    pub(crate) fn gep_type_to_basic_type(&self, ty: &GepType) -> Result<BasicTypeEnum<'ctx>> {
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

    /// Build return type for function declarations
    pub(crate) fn build_return_type(
        &self,
        return_type: &ReturnType,
        param_types: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
        varargs: bool,
    ) -> inkwell::types::FunctionType<'ctx> {
        match return_type {
            ReturnType::Scalar(ScalarType::Void) => {
                self.context.void_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::I1) => {
                self.context.bool_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::I8) => {
                self.context.i8_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::I16) => {
                self.context.i16_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::I32) => {
                self.context.i32_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::I64) => {
                self.context.i64_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::Float) => {
                self.context.f32_type().fn_type(param_types, varargs)
            }
            ReturnType::Scalar(ScalarType::Double) => {
                self.context.f64_type().fn_type(param_types, varargs)
            }
            ReturnType::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(param_types, varargs),
            ReturnType::AnonStruct(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .filter_map(|f| self.param_type_to_basic_type(f))
                    .collect();
                let struct_type = self.context.struct_type(&field_types, false);
                struct_type.fn_type(param_types, varargs)
            }
        }
    }
}
