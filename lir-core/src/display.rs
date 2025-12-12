//! Display implementations for lIR AST types
//!
//! Provides pretty-printing of lIR AST back to S-expression syntax.

use crate::ast::*;
use std::fmt::{self, Display, Formatter};

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Literals
            Expr::IntLit { ty, value } => write!(f, "({} {})", ty, value),
            Expr::FloatLit { ty, value } => {
                let val = match value {
                    FloatValue::Number(n) => format!("{}", n),
                    FloatValue::Inf => "inf".to_string(),
                    FloatValue::NegInf => "-inf".to_string(),
                    FloatValue::Nan => "nan".to_string(),
                };
                write!(f, "({} {})", ty, val)
            }
            Expr::VectorLit { ty, elements } => {
                write!(f, "(vector {} ", ty)?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Expr::NullPtr => write!(f, "(ptr null)"),
            Expr::StringLit(s) => write!(f, "(string {:?})", s),
            Expr::StructLit(fields) => {
                write!(f, "{{ ")?;
                for (i, e) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, " }}")
            }

            // Aggregate operations
            Expr::ExtractValue { aggregate, indices } => {
                write!(f, "(extractvalue {} ", aggregate)?;
                for idx in indices {
                    write!(f, "{} ", idx)?;
                }
                write!(f, ")")
            }
            Expr::InsertValue {
                aggregate,
                value,
                indices,
            } => {
                write!(f, "(insertvalue {} {} ", aggregate, value)?;
                for idx in indices {
                    write!(f, "{} ", idx)?;
                }
                write!(f, ")")
            }

            // Integer arithmetic
            Expr::Add(a, b) => write!(f, "(add {} {})", a, b),
            Expr::Sub(a, b) => write!(f, "(sub {} {})", a, b),
            Expr::Mul(a, b) => write!(f, "(mul {} {})", a, b),
            Expr::SDiv(a, b) => write!(f, "(sdiv {} {})", a, b),
            Expr::UDiv(a, b) => write!(f, "(udiv {} {})", a, b),
            Expr::SRem(a, b) => write!(f, "(srem {} {})", a, b),
            Expr::URem(a, b) => write!(f, "(urem {} {})", a, b),

            // Float arithmetic
            Expr::FAdd(a, b) => write!(f, "(fadd {} {})", a, b),
            Expr::FSub(a, b) => write!(f, "(fsub {} {})", a, b),
            Expr::FMul(a, b) => write!(f, "(fmul {} {})", a, b),
            Expr::FDiv(a, b) => write!(f, "(fdiv {} {})", a, b),
            Expr::FRem(a, b) => write!(f, "(frem {} {})", a, b),

            // Bitwise
            Expr::And(a, b) => write!(f, "(and {} {})", a, b),
            Expr::Or(a, b) => write!(f, "(or {} {})", a, b),
            Expr::Xor(a, b) => write!(f, "(xor {} {})", a, b),
            Expr::Shl(a, b) => write!(f, "(shl {} {})", a, b),
            Expr::LShr(a, b) => write!(f, "(lshr {} {})", a, b),
            Expr::AShr(a, b) => write!(f, "(ashr {} {})", a, b),
            Expr::Ctpop(v) => write!(f, "(ctpop {})", v),

            // Comparisons
            Expr::ICmp { pred, lhs, rhs } => write!(f, "(icmp {} {} {})", pred, lhs, rhs),
            Expr::FCmp { pred, lhs, rhs } => write!(f, "(fcmp {} {} {})", pred, lhs, rhs),

            // Conversions
            Expr::Trunc { ty, value } => write!(f, "(trunc {} {})", ty, value),
            Expr::ZExt { ty, value } => write!(f, "(zext {} {})", ty, value),
            Expr::SExt { ty, value } => write!(f, "(sext {} {})", ty, value),
            Expr::FPTrunc { ty, value } => write!(f, "(fptrunc {} {})", ty, value),
            Expr::FPExt { ty, value } => write!(f, "(fpext {} {})", ty, value),
            Expr::FPToUI { ty, value } => write!(f, "(fptoui {} {})", ty, value),
            Expr::FPToSI { ty, value } => write!(f, "(fptosi {} {})", ty, value),
            Expr::UIToFP { ty, value } => write!(f, "(uitofp {} {})", ty, value),
            Expr::SIToFP { ty, value } => write!(f, "(sitofp {} {})", ty, value),

            // Control flow
            Expr::Select {
                cond,
                true_val,
                false_val,
            } => write!(f, "(select {} {} {})", cond, true_val, false_val),

            // Vector operations
            Expr::ExtractElement { vec, idx } => write!(f, "(extractelement {} {})", vec, idx),
            Expr::InsertElement { vec, val, idx } => {
                write!(f, "(insertelement {} {} {})", vec, val, idx)
            }
            Expr::ShuffleVector { vec1, vec2, mask } => {
                write!(f, "(shufflevector {} {} {})", vec1, vec2, mask)
            }

            // Local variable reference
            Expr::LocalRef(name) => write!(f, "{}", name),

            // Global/function reference (for function pointers)
            Expr::GlobalRef(name) => write!(f, "@{}", name),

            // Return instruction
            Expr::Ret(Some(val)) => write!(f, "(ret {})", val),
            Expr::Ret(None) => write!(f, "(ret)"),

            // Memory operations
            Expr::Alloca { ty, count: None } => write!(f, "(alloca {})", ty),
            Expr::Alloca { ty, count: Some(n) } => write!(f, "(alloca {} {})", ty, n),
            Expr::Load { ty, ptr } => write!(f, "(load {} {})", ty, ptr),
            Expr::Store { value, ptr } => write!(f, "(store {} {})", value, ptr),

            // Ownership operations
            Expr::AllocOwn { elem_type } => write!(f, "(alloc own {})", elem_type),
            Expr::BorrowRef { value } => write!(f, "(borrow ref {})", value),
            Expr::BorrowRefMut { value } => write!(f, "(borrow refmut {})", value),
            Expr::Drop { value } => write!(f, "(drop {})", value),
            Expr::Move { value } => write!(f, "(move {})", value),

            // Pointer arithmetic
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                if *inbounds {
                    write!(f, "(getelementptr inbounds {} {}", ty, ptr)?;
                } else {
                    write!(f, "(getelementptr {} {}", ty, ptr)?;
                }
                for idx in indices {
                    write!(f, " {}", idx)?;
                }
                write!(f, ")")
            }

            // Control flow
            Expr::Br(target) => match target {
                BranchTarget::Unconditional(label) => write!(f, "(br {})", label),
                BranchTarget::Conditional {
                    cond,
                    true_label,
                    false_label,
                } => write!(f, "(br {} {} {})", cond, true_label, false_label),
            },
            Expr::Phi { ty, incoming } => {
                write!(f, "(phi {}", ty)?;
                for (label, val) in incoming {
                    write!(f, " ({} {})", label, val)?;
                }
                write!(f, ")")
            }

            // Function call
            Expr::Call { name, args } => {
                write!(f, "(call @{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Expr::TailCall { name, args } => {
                write!(f, "(tailcall @{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Expr::IndirectCall {
                fn_ptr,
                ret_ty,
                args,
            } => {
                write!(f, "(indirect-call {} {}", fn_ptr, ret_ty)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }

            // Array operations
            Expr::ArrayAlloc { elem_type, size } => {
                write!(f, "(array-alloc {} {})", elem_type, size)
            }
            Expr::ArrayGet {
                elem_type,
                size,
                array,
                index,
            } => write!(f, "(array-get {} {} {} {})", elem_type, size, array, index),
            Expr::ArraySet {
                elem_type,
                size,
                array,
                index,
                value,
            } => write!(
                f,
                "(array-set {} {} {} {} {})",
                elem_type, size, array, index, value
            ),
            Expr::ArrayLen { size } => write!(f, "(array-len {})", size),
            Expr::ArrayPtr { array } => write!(f, "(array-ptr {})", array),

            // Heap-allocated struct with ownership
            Expr::HeapStruct {
                struct_name,
                fields,
            } => {
                write!(f, "(heap-struct {}", struct_name)?;
                for field in fields {
                    write!(f, " {}", field)?;
                }
                write!(f, ")")
            }

            // Reference counting
            Expr::RcAlloc { elem_type } => write!(f, "(rc-alloc {})", elem_type),
            Expr::RcClone { value } => write!(f, "(rc-clone {})", value),
            Expr::RcDrop { value } => write!(f, "(rc-drop {})", value),
            Expr::RcCount { value } => write!(f, "(rc-count {})", value),
            Expr::RcPtr { value } => write!(f, "(rc-ptr {})", value),

            // Memory deallocation
            Expr::Free { ptr } => write!(f, "(free {})", ptr),

            // Atomic operations
            Expr::AtomicLoad { ordering, ty, ptr } => {
                write!(f, "(atomic-load {} {} {})", ordering, ty, ptr)
            }
            Expr::AtomicStore {
                ordering,
                value,
                ptr,
            } => write!(f, "(atomic-store {} {} {})", ordering, value, ptr),
            Expr::AtomicRMW {
                op,
                ordering,
                ptr,
                value,
            } => write!(f, "(atomicrmw {} {} {} {})", op, ordering, ptr, value),
            Expr::CmpXchg {
                ordering,
                ptr,
                expected,
                new_value,
            } => write!(
                f,
                "(cmpxchg {} {} {} {})",
                ordering, ptr, expected, new_value
            ),

            // Fence
            Expr::Fence { ordering } => write!(f, "(fence {})", ordering),

            // Let binding
            Expr::Let { bindings, body } => {
                write!(f, "(let (")?;
                for (i, (name, val)) in bindings.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "({} {})", name, val)?;
                }
                write!(f, ")")?;
                for expr in body {
                    write!(f, " {}", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(block {}", self.label)?;
        for instr in &self.instructions {
            write!(f, " {}", instr)?;
        }
        write!(f, ")")
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.ty, self.name)
    }
}

impl Display for FunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(define ({} {}) (", self.name, self.return_type)?;
        for (i, p) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", p)?;
        }
        write!(f, ")")?;
        for block in &self.blocks {
            write!(f, " {}", block)?;
        }
        write!(f, ")")
    }
}

impl Display for ExternDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(declare {} {} (", self.name, self.return_type)?;
        for (i, ty) in self.param_types.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", ty)?;
        }
        if self.varargs {
            write!(f, " ...")?;
        }
        write!(f, "))")
    }
}

impl Display for GlobalDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let kind = if self.is_constant {
            "constant"
        } else {
            "global"
        };
        write!(
            f,
            "({} @{} {} {})",
            kind, self.name, self.ty, self.initializer
        )
    }
}

impl Display for StructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(defstruct {} (", self.name)?;
        for (i, ty) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, "))")
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Item::Function(func) => write!(f, "{}", func),
            Item::ExternDecl(decl) => write!(f, "{}", decl),
            Item::Global(global) => write!(f, "{}", global),
            Item::Struct(s) => write!(f, "{}", s),
        }
    }
}

/// A module containing top-level items
pub struct Module {
    pub items: Vec<Item>,
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}
