//! Borrow checker for lIR
//!
//! Verifies ownership and borrowing rules before codegen.
//! Rejects programs that violate memory safety rules.

use crate::ast::*;
use std::collections::HashMap;
use thiserror::Error;

/// Borrow checking errors
#[derive(Error, Debug, Clone, PartialEq)]
pub enum BorrowError {
    #[error("use of moved value `{0}`")]
    UseAfterMove(String),

    #[error("use of dropped value `{0}`")]
    UseAfterDrop(String),

    #[error("cannot borrow `{0}` as immutable because it is already borrowed as mutable")]
    BorrowConflictMutToShared(String),

    #[error("cannot borrow `{0}` as mutable because it is already borrowed")]
    BorrowConflictToMut(String),

    #[error("cannot borrow `{0}` as mutable more than once")]
    DoubleMutableBorrow(String),

    #[error("cannot move `{0}` while borrowed")]
    MoveWhileBorrowed(String),

    #[error("cannot drop `{0}` while borrowed")]
    DropWhileBorrowed(String),

    #[error("value `{0}` not dropped before scope exit")]
    NotDropped(String),

    #[error("borrow of `{0}` escapes scope")]
    BorrowEscapes(String),

    #[error("undefined variable: {0}")]
    Undefined(String),
}

/// Ownership state of a binding
#[derive(Debug, Clone, PartialEq)]
pub enum OwnershipState {
    /// Value is owned and valid
    Owned,
    /// Value has been moved to another binding
    Moved,
    /// Value has been dropped
    Dropped,
    /// Value is borrowed (shared borrows)
    Borrowed { count: usize },
    /// Value is mutably borrowed
    BorrowedMut,
}

/// Information about a borrow
#[derive(Debug, Clone)]
pub struct BorrowInfo {
    /// Whether this is a mutable borrow
    pub is_mut: bool,
    /// The source binding being borrowed
    pub source: String,
    /// Scope depth when borrow was created
    pub scope_depth: usize,
}

/// Borrow checker state
pub struct BorrowChecker {
    /// Ownership state of each binding
    bindings: HashMap<String, OwnershipState>,
    /// Active borrows (borrow binding -> info)
    borrows: HashMap<String, BorrowInfo>,
    /// Current scope depth
    scope_depth: usize,
    /// Collected errors
    errors: Vec<BorrowError>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            borrows: HashMap::new(),
            scope_depth: 0,
            errors: Vec::new(),
        }
    }

    /// Check a function definition
    pub fn check_function(&mut self, func: &FunctionDef) -> Result<(), Vec<BorrowError>> {
        // Reset state
        self.bindings.clear();
        self.borrows.clear();
        self.scope_depth = 0;
        self.errors.clear();

        // Initialize parameters
        for param in &func.params {
            let state = match &param.ty {
                ParamType::Own(_) => OwnershipState::Owned,
                ParamType::Ref(_) => OwnershipState::Borrowed { count: 1 },
                ParamType::RefMut(_) => OwnershipState::BorrowedMut,
                ParamType::Scalar(_)
                | ParamType::Ptr
                | ParamType::Rc(_)
                | ParamType::AnonStruct(_) => continue, // Non-ownership types (RC has its own tracking)
            };
            self.bindings.insert(param.name.clone(), state);
        }

        // Check each block
        for block in &func.blocks {
            self.check_block(block);
        }

        // Verify all owned values are dropped at function end
        // (except those that are returned)
        self.verify_cleanup();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_block(&mut self, block: &BasicBlock) {
        for inst in &block.instructions {
            self.check_expr(inst);
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            // Let bindings create new scope
            Expr::Let { bindings, body } => {
                self.scope_depth += 1;
                let scope_start = self.scope_depth;

                // Process bindings
                for (name, value_expr) in bindings {
                    self.check_expr(value_expr);

                    // Determine ownership state based on the expression
                    let state = self.ownership_from_expr(value_expr, name);
                    if let Some(s) = state {
                        self.bindings.insert(name.clone(), s);
                    }
                }

                // Process body
                for body_expr in body {
                    self.check_expr(body_expr);
                }

                // End scope - invalidate borrows created in this scope
                self.end_scope(scope_start);
                self.scope_depth -= 1;
            }

            // Ownership operations
            Expr::AllocOwn { .. } => {
                // Creates a new owned value - tracked when bound in Let
            }

            Expr::BorrowRef { value } => {
                if let Expr::LocalRef(name) = value.as_ref() {
                    self.check_shared_borrow(name);
                }
            }

            Expr::BorrowRefMut { value } => {
                if let Expr::LocalRef(name) = value.as_ref() {
                    self.check_mut_borrow(name);
                }
            }

            Expr::Drop { value } => {
                if let Expr::LocalRef(name) = value.as_ref() {
                    self.check_drop(name);
                }
            }

            Expr::Move { value } => {
                if let Expr::LocalRef(name) = value.as_ref() {
                    self.check_move(name);
                }
            }

            // Local reference - check it's valid to use
            Expr::LocalRef(name) => {
                self.check_use(name);
            }

            // Recurse into sub-expressions
            Expr::Load { ptr, .. } => self.check_expr(ptr),
            Expr::Store { value, ptr } => {
                self.check_expr(value);
                self.check_expr(ptr);
            }
            Expr::Add(a, b)
            | Expr::Sub(a, b)
            | Expr::Mul(a, b)
            | Expr::SDiv(a, b)
            | Expr::UDiv(a, b)
            | Expr::SRem(a, b)
            | Expr::URem(a, b)
            | Expr::FAdd(a, b)
            | Expr::FSub(a, b)
            | Expr::FMul(a, b)
            | Expr::FDiv(a, b)
            | Expr::FRem(a, b)
            | Expr::And(a, b)
            | Expr::Or(a, b)
            | Expr::Xor(a, b)
            | Expr::Shl(a, b)
            | Expr::LShr(a, b)
            | Expr::AShr(a, b) => {
                self.check_expr(a);
                self.check_expr(b);
            }

            Expr::Ctpop(v) => {
                self.check_expr(v);
            }

            Expr::ICmp { lhs, rhs, .. } | Expr::FCmp { lhs, rhs, .. } => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }

            Expr::Select {
                cond,
                true_val,
                false_val,
            } => {
                self.check_expr(cond);
                self.check_expr(true_val);
                self.check_expr(false_val);
            }

            Expr::Trunc { value, .. }
            | Expr::ZExt { value, .. }
            | Expr::SExt { value, .. }
            | Expr::FPTrunc { value, .. }
            | Expr::FPExt { value, .. }
            | Expr::FPToUI { value, .. }
            | Expr::FPToSI { value, .. }
            | Expr::UIToFP { value, .. }
            | Expr::SIToFP { value, .. } => {
                self.check_expr(value);
            }

            Expr::Call { args, .. } | Expr::TailCall { args, .. } => {
                for arg in args {
                    self.check_expr(arg);
                }
            }

            Expr::IndirectCall { fn_ptr, args, .. } => {
                self.check_expr(fn_ptr);
                for arg in args {
                    self.check_expr(arg);
                }
            }

            Expr::Ret(Some(value)) => self.check_expr(value),
            Expr::Ret(None) => {}

            Expr::Br(BranchTarget::Conditional { cond, .. }) => {
                self.check_expr(cond);
            }
            Expr::Br(BranchTarget::Unconditional(_)) => {}

            Expr::Phi { incoming, .. } => {
                for (_, value) in incoming {
                    self.check_expr(value);
                }
            }

            Expr::GetElementPtr { ptr, indices, .. } => {
                self.check_expr(ptr);
                for idx in indices {
                    self.check_expr(idx);
                }
            }

            Expr::Alloca { count, .. } => {
                if let Some(c) = count {
                    self.check_expr(c);
                }
            }

            Expr::ExtractElement { vec, idx } => {
                self.check_expr(vec);
                self.check_expr(idx);
            }

            Expr::InsertElement { vec, val, idx } => {
                self.check_expr(vec);
                self.check_expr(val);
                self.check_expr(idx);
            }

            Expr::ShuffleVector { vec1, vec2, mask } => {
                self.check_expr(vec1);
                self.check_expr(vec2);
                self.check_expr(mask);
            }

            Expr::ExtractValue { aggregate, .. } => {
                self.check_expr(aggregate);
            }

            Expr::InsertValue {
                aggregate, value, ..
            } => {
                self.check_expr(aggregate);
                self.check_expr(value);
            }

            Expr::ArrayAlloc { .. } | Expr::ArrayLen { .. } => {}

            Expr::ArrayGet { array, index, .. } => {
                self.check_expr(array);
                self.check_expr(index);
            }

            Expr::ArraySet {
                array,
                index,
                value,
                ..
            } => {
                self.check_expr(array);
                self.check_expr(index);
                self.check_expr(value);
            }

            Expr::ArrayPtr { array } => {
                self.check_expr(array);
            }

            Expr::StructLit(fields) => {
                for field in fields {
                    self.check_expr(field);
                }
            }

            Expr::VectorLit { elements, .. } => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }

            // Literals don't need checking
            Expr::IntLit { .. } | Expr::FloatLit { .. } | Expr::NullPtr | Expr::StringLit(_) => {}

            // RC operations - not tracked by borrow checker (has its own refcount semantics)
            Expr::RcAlloc { .. } => {}
            Expr::RcClone { value } => {
                self.check_expr(value);
            }
            Expr::RcDrop { value } => {
                self.check_expr(value);
            }
            Expr::RcCount { value } => {
                self.check_expr(value);
            }
            Expr::RcPtr { value } => {
                self.check_expr(value);
            }

            // Memory deallocation
            Expr::Free { ptr } => {
                self.check_expr(ptr);
            }

            // Heap-allocated struct - check field expressions
            Expr::HeapStruct { fields, .. } => {
                for field in fields {
                    self.check_expr(field);
                }
            }

            // Atomic memory operations - check pointer expression
            Expr::AtomicLoad { ptr, .. } => {
                self.check_expr(ptr);
            }
            Expr::AtomicStore { value, ptr, .. } => {
                self.check_expr(value);
                self.check_expr(ptr);
            }
            Expr::AtomicRMW { ptr, value, .. } => {
                self.check_expr(ptr);
                self.check_expr(value);
            }
            Expr::CmpXchg {
                ptr,
                expected,
                new_value,
                ..
            } => {
                self.check_expr(ptr);
                self.check_expr(expected);
                self.check_expr(new_value);
            }
            Expr::Fence { .. } => {
                // Fence has no operands to check
            }
            Expr::GlobalRef(_) => {
                // Global references are always valid, nothing to check
            }
        }
    }

    /// Determine ownership state from an expression
    fn ownership_from_expr(&mut self, expr: &Expr, binding_name: &str) -> Option<OwnershipState> {
        match expr {
            Expr::AllocOwn { .. } => Some(OwnershipState::Owned),

            Expr::BorrowRef { value } => {
                if let Expr::LocalRef(source) = value.as_ref() {
                    self.borrows.insert(
                        binding_name.to_string(),
                        BorrowInfo {
                            is_mut: false,
                            source: source.clone(),
                            scope_depth: self.scope_depth,
                        },
                    );
                }
                Some(OwnershipState::Borrowed { count: 1 })
            }

            Expr::BorrowRefMut { value } => {
                if let Expr::LocalRef(source) = value.as_ref() {
                    self.borrows.insert(
                        binding_name.to_string(),
                        BorrowInfo {
                            is_mut: true,
                            source: source.clone(),
                            scope_depth: self.scope_depth,
                        },
                    );
                }
                Some(OwnershipState::BorrowedMut)
            }

            Expr::Move { value } => {
                if let Expr::LocalRef(source) = value.as_ref() {
                    // Mark source as moved
                    if let Some(state) = self.bindings.get_mut(source) {
                        *state = OwnershipState::Moved;
                    }
                }
                Some(OwnershipState::Owned)
            }

            // Local reference as RHS is a move for owned values
            Expr::LocalRef(source) => {
                if let Some(OwnershipState::Owned) = self.bindings.get(source) {
                    // This is an implicit move
                    if let Some(state) = self.bindings.get_mut(source) {
                        *state = OwnershipState::Moved;
                    }
                    Some(OwnershipState::Owned)
                } else {
                    None // Non-owned types just copy
                }
            }

            _ => None, // Other expressions don't transfer ownership
        }
    }

    fn check_use(&mut self, name: &str) {
        match self.bindings.get(name) {
            Some(OwnershipState::Moved) => {
                self.errors
                    .push(BorrowError::UseAfterMove(name.to_string()));
            }
            Some(OwnershipState::Dropped) => {
                self.errors
                    .push(BorrowError::UseAfterDrop(name.to_string()));
            }
            None => {
                // Not an ownership-tracked binding, that's fine
            }
            Some(_) => {
                // Valid use
            }
        }
    }

    fn check_shared_borrow(&mut self, source: &str) {
        // Check source is valid
        match self.bindings.get(source) {
            Some(OwnershipState::Moved) => {
                self.errors
                    .push(BorrowError::UseAfterMove(source.to_string()));
                return;
            }
            Some(OwnershipState::Dropped) => {
                self.errors
                    .push(BorrowError::UseAfterDrop(source.to_string()));
                return;
            }
            Some(OwnershipState::BorrowedMut) => {
                self.errors
                    .push(BorrowError::BorrowConflictMutToShared(source.to_string()));
                return;
            }
            None => {
                // Not tracked, allow
                return;
            }
            Some(_) => {}
        }

        // Check if already mutably borrowed
        for info in self.borrows.values() {
            if info.source == source && info.is_mut {
                self.errors
                    .push(BorrowError::BorrowConflictMutToShared(source.to_string()));
                return;
            }
        }

        // Update source state
        if let Some(state) = self.bindings.get_mut(source) {
            match state {
                OwnershipState::Owned => {
                    *state = OwnershipState::Borrowed { count: 1 };
                }
                OwnershipState::Borrowed { count } => {
                    *count += 1;
                }
                _ => {}
            }
        }
    }

    fn check_mut_borrow(&mut self, source: &str) {
        // Check source is valid
        match self.bindings.get(source) {
            Some(OwnershipState::Moved) => {
                self.errors
                    .push(BorrowError::UseAfterMove(source.to_string()));
                return;
            }
            Some(OwnershipState::Dropped) => {
                self.errors
                    .push(BorrowError::UseAfterDrop(source.to_string()));
                return;
            }
            Some(OwnershipState::BorrowedMut) => {
                self.errors
                    .push(BorrowError::DoubleMutableBorrow(source.to_string()));
                return;
            }
            Some(OwnershipState::Borrowed { .. }) => {
                self.errors
                    .push(BorrowError::BorrowConflictToMut(source.to_string()));
                return;
            }
            None => {
                // Not tracked, allow
                return;
            }
            Some(OwnershipState::Owned) => {}
        }

        // Check if already borrowed
        for info in self.borrows.values() {
            if info.source == source {
                if info.is_mut {
                    self.errors
                        .push(BorrowError::DoubleMutableBorrow(source.to_string()));
                } else {
                    self.errors
                        .push(BorrowError::BorrowConflictToMut(source.to_string()));
                }
                return;
            }
        }

        // Update source state
        if let Some(state) = self.bindings.get_mut(source) {
            *state = OwnershipState::BorrowedMut;
        }
    }

    fn check_move(&mut self, name: &str) {
        match self.bindings.get(name) {
            Some(OwnershipState::Moved) => {
                self.errors
                    .push(BorrowError::UseAfterMove(name.to_string()));
            }
            Some(OwnershipState::Dropped) => {
                self.errors
                    .push(BorrowError::UseAfterDrop(name.to_string()));
            }
            Some(OwnershipState::Borrowed { .. }) | Some(OwnershipState::BorrowedMut) => {
                self.errors
                    .push(BorrowError::MoveWhileBorrowed(name.to_string()));
            }
            Some(OwnershipState::Owned) => {
                // Check if any borrows exist from this value
                for info in self.borrows.values() {
                    if info.source == name {
                        self.errors
                            .push(BorrowError::MoveWhileBorrowed(name.to_string()));
                        return;
                    }
                }
            }
            None => {}
        }
    }

    fn check_drop(&mut self, name: &str) {
        match self.bindings.get(name) {
            Some(OwnershipState::Moved) => {
                self.errors
                    .push(BorrowError::UseAfterMove(name.to_string()));
            }
            Some(OwnershipState::Dropped) => {
                self.errors
                    .push(BorrowError::UseAfterDrop(name.to_string()));
            }
            Some(OwnershipState::Borrowed { .. }) | Some(OwnershipState::BorrowedMut) => {
                self.errors
                    .push(BorrowError::DropWhileBorrowed(name.to_string()));
            }
            Some(OwnershipState::Owned) => {
                // Check if any borrows exist from this value
                for info in self.borrows.values() {
                    if info.source == name {
                        self.errors
                            .push(BorrowError::DropWhileBorrowed(name.to_string()));
                        return;
                    }
                }
                // Mark as dropped
                if let Some(state) = self.bindings.get_mut(name) {
                    *state = OwnershipState::Dropped;
                }
            }
            None => {}
        }
    }

    fn end_scope(&mut self, scope_start: usize) {
        // Remove borrows created in this scope
        let to_remove: Vec<String> = self
            .borrows
            .iter()
            .filter(|(_, info)| info.scope_depth >= scope_start)
            .map(|(name, _)| name.clone())
            .collect();

        for borrow_name in &to_remove {
            if let Some(info) = self.borrows.remove(borrow_name) {
                // Decrement borrow count on source
                if let Some(state) = self.bindings.get_mut(&info.source) {
                    match state {
                        OwnershipState::Borrowed { count } if *count > 1 => {
                            *count -= 1;
                        }
                        OwnershipState::Borrowed { .. } => {
                            *state = OwnershipState::Owned;
                        }
                        OwnershipState::BorrowedMut => {
                            *state = OwnershipState::Owned;
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn verify_cleanup(&mut self) {
        // At function end, all owned values should be dropped or moved
        for (name, state) in &self.bindings {
            if *state == OwnershipState::Owned {
                self.errors.push(BorrowError::NotDropped(name.clone()));
            }
        }
    }
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn check_function(input: &str) -> Result<(), Vec<BorrowError>> {
        let mut parser = Parser::new(input);
        let func = match parser.parse_item() {
            Ok(crate::parser::ParseResult::Function(f)) => f,
            other => panic!("Expected function, got {:?}", other),
        };
        BorrowChecker::new().check_function(&func)
    }

    #[test]
    fn test_valid_alloc_and_drop() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (drop x) (ret))))",
        );
        assert!(result.is_ok(), "Expected Ok, got {:?}", result);
    }

    #[test]
    fn test_use_after_move() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (let ((y (move x))) (load i64 x)) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::UseAfterMove(n) if n == "x")));
    }

    #[test]
    fn test_use_after_drop() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (drop x) (load i64 x) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::UseAfterDrop(n) if n == "x")));
    }

    #[test]
    fn test_double_mutable_borrow() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow refmut x))) (ret)) (drop x) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::DoubleMutableBorrow(s) if s == "x")));
    }

    #[test]
    fn test_mutable_then_shared_borrow() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow ref x))) (ret)) (drop x) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::BorrowConflictMutToShared(s) if s == "x")));
    }

    #[test]
    fn test_shared_then_mutable_borrow() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow refmut x))) (ret)) (drop x) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::BorrowConflictToMut(s) if s == "x")));
    }

    #[test]
    fn test_multiple_shared_borrows_ok() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow ref x))) (add (load i64 a) (load i64 b))) (drop x) (ret))))",
        );
        assert!(result.is_ok(), "Expected Ok, got {:?}", result);
    }

    #[test]
    fn test_not_dropped() {
        let result = check_function(
            "(define (test void) () (block entry (let ((x (alloc own i64))) (ret))))",
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, BorrowError::NotDropped(n) if n == "x")));
    }
}
