//! Ownership and borrow checking
//!
//! This is the core of liar's memory safety: ensuring that:
//! - Values are not used after move
//! - Mutable references are exclusive
//! - References do not outlive their referents
//!
//! Based on ADRs 001-007

use std::collections::HashMap;

use crate::ast::{Def, Defun, Expr, ExtendProtocol, Item, LetBinding, MatchArm, Program};
use crate::error::{CompileError, Errors};
use crate::resolve::BindingId;
use crate::span::{Span, Spanned};

/// Unique identifier for a borrow
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BorrowId(pub u32);

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

/// Ownership state of a value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnershipState {
    /// Value is owned here
    Owned,
    /// Value has been moved away
    Moved(Span),
    /// Immutably borrowed
    Borrowed(Vec<BorrowId>),
    /// Mutably borrowed (exclusive)
    BorrowedMut(BorrowId),
}

/// Kind of borrow
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorrowKind {
    Shared,
    Mutable,
}

/// Information about an active borrow
#[derive(Debug, Clone)]
pub struct BorrowInfo {
    pub kind: BorrowKind,
    pub borrowed_from: BindingId,
    pub span: Span,
    pub scope: ScopeId,
}

/// A scope in the borrow checker
#[derive(Debug)]
struct Scope {
    id: ScopeId,
    /// Borrows that will end when this scope ends
    borrows: Vec<BorrowId>,
    /// Variables introduced in this scope
    bindings: Vec<BindingId>,
}

/// Borrow checker state
pub struct BorrowChecker {
    /// Current scope stack
    scopes: Vec<Scope>,
    /// Ownership state of each binding
    ownership: HashMap<BindingId, OwnershipState>,
    /// Active borrows
    borrows: HashMap<BorrowId, BorrowInfo>,
    /// Next borrow ID
    next_borrow_id: u32,
    /// Next scope ID
    next_scope_id: u32,
    /// Collected errors
    errors: Errors,
    /// Map from variable names to binding IDs (for lookups)
    bindings: HashMap<String, Vec<BindingId>>,
    /// Next binding ID
    next_binding_id: u32,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                id: ScopeId(0),
                borrows: Vec::new(),
                bindings: Vec::new(),
            }],
            ownership: HashMap::new(),
            borrows: HashMap::new(),
            next_borrow_id: 0,
            next_scope_id: 1,
            errors: Errors::new(),
            bindings: HashMap::new(),
            next_binding_id: 0,
        }
    }

    fn fresh_borrow_id(&mut self) -> BorrowId {
        let id = BorrowId(self.next_borrow_id);
        self.next_borrow_id += 1;
        id
    }

    fn fresh_scope_id(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    fn fresh_binding_id(&mut self) -> BindingId {
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;
        id
    }

    fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("no scope")
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("no scope")
    }

    fn push_scope(&mut self) {
        let id = self.fresh_scope_id();
        self.scopes.push(Scope {
            id,
            borrows: Vec::new(),
            bindings: Vec::new(),
        });
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // End all borrows in this scope
            for borrow_id in &scope.borrows {
                self.end_borrow(*borrow_id);
            }

            // Drop all bindings in this scope
            for binding_id in &scope.bindings {
                // Remove from the bindings stack
                for (_name, ids) in self.bindings.iter_mut() {
                    ids.retain(|id| id != binding_id);
                }
                // Remove ownership tracking
                self.ownership.remove(binding_id);
            }
        }
    }

    /// Define a new binding in the current scope
    fn define(&mut self, name: &str, _span: Span) -> BindingId {
        let id = self.fresh_binding_id();
        self.ownership.insert(id, OwnershipState::Owned);
        self.current_scope_mut().bindings.push(id);

        // Add to name lookup
        self.bindings.entry(name.to_string()).or_default().push(id);

        id
    }

    /// Look up a binding by name
    fn lookup(&self, name: &str) -> Option<BindingId> {
        self.bindings.get(name).and_then(|ids| ids.last().copied())
    }

    /// Use a value (may move it for owned types)
    fn use_value(&mut self, name: &str, span: Span) {
        if let Some(binding_id) = self.lookup(name) {
            match self.ownership.get(&binding_id).cloned() {
                Some(OwnershipState::Moved(move_span)) => {
                    self.errors.push(CompileError::borrow(
                        span,
                        format!(
                            "use of moved value '{}' (moved at {}..{})",
                            name, move_span.start, move_span.end
                        ),
                    ));
                }
                Some(OwnershipState::BorrowedMut(borrow_id)) => {
                    if let Some(info) = self.borrows.get(&borrow_id) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot use '{}' while mutably borrowed (borrowed at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                }
                Some(OwnershipState::Owned) | Some(OwnershipState::Borrowed(_)) | None => {
                    // OK to use
                }
            }
        }
    }

    /// Move a value
    fn move_value(&mut self, name: &str, span: Span) {
        if let Some(binding_id) = self.lookup(name) {
            match self.ownership.get(&binding_id).cloned() {
                Some(OwnershipState::Moved(move_span)) => {
                    self.errors.push(CompileError::borrow(
                        span,
                        format!(
                            "use of moved value '{}' (already moved at {}..{})",
                            name, move_span.start, move_span.end
                        ),
                    ));
                }
                Some(OwnershipState::Borrowed(ref borrow_ids)) if !borrow_ids.is_empty() => {
                    if let Some(info) = borrow_ids.first().and_then(|id| self.borrows.get(id)) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot move '{}' while borrowed (borrowed at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                }
                Some(OwnershipState::BorrowedMut(borrow_id)) => {
                    if let Some(info) = self.borrows.get(&borrow_id) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot move '{}' while mutably borrowed (borrowed at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                }
                Some(OwnershipState::Owned) | Some(OwnershipState::Borrowed(_)) | None => {
                    self.ownership
                        .insert(binding_id, OwnershipState::Moved(span));
                }
            }
        }
    }

    /// Create a shared borrow
    fn borrow_shared(&mut self, name: &str, span: Span) -> Option<BorrowId> {
        if let Some(binding_id) = self.lookup(name) {
            match self.ownership.get(&binding_id).cloned() {
                Some(OwnershipState::Moved(move_span)) => {
                    self.errors.push(CompileError::borrow(
                        span,
                        format!(
                            "cannot borrow '{}' as it was moved (moved at {}..{})",
                            name, move_span.start, move_span.end
                        ),
                    ));
                    None
                }
                Some(OwnershipState::BorrowedMut(borrow_id)) => {
                    if let Some(info) = self.borrows.get(&borrow_id) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot borrow '{}' as immutable because it is already borrowed as mutable (at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                    None
                }
                Some(OwnershipState::Owned) => {
                    let borrow_id = self.fresh_borrow_id();
                    let scope_id = self.current_scope().id;
                    self.borrows.insert(
                        borrow_id,
                        BorrowInfo {
                            kind: BorrowKind::Shared,
                            borrowed_from: binding_id,
                            span,
                            scope: scope_id,
                        },
                    );
                    self.ownership
                        .insert(binding_id, OwnershipState::Borrowed(vec![borrow_id]));
                    self.current_scope_mut().borrows.push(borrow_id);
                    Some(borrow_id)
                }
                Some(OwnershipState::Borrowed(mut borrow_ids)) => {
                    // Additional shared borrow is OK
                    let borrow_id = self.fresh_borrow_id();
                    let scope_id = self.current_scope().id;
                    self.borrows.insert(
                        borrow_id,
                        BorrowInfo {
                            kind: BorrowKind::Shared,
                            borrowed_from: binding_id,
                            span,
                            scope: scope_id,
                        },
                    );
                    borrow_ids.push(borrow_id);
                    self.ownership
                        .insert(binding_id, OwnershipState::Borrowed(borrow_ids));
                    self.current_scope_mut().borrows.push(borrow_id);
                    Some(borrow_id)
                }
                None => None,
            }
        } else {
            None
        }
    }

    /// Create a mutable borrow
    fn borrow_mut(&mut self, name: &str, span: Span) -> Option<BorrowId> {
        if let Some(binding_id) = self.lookup(name) {
            match self.ownership.get(&binding_id).cloned() {
                Some(OwnershipState::Moved(move_span)) => {
                    self.errors.push(CompileError::borrow(
                        span,
                        format!(
                            "cannot borrow '{}' as it was moved (moved at {}..{})",
                            name, move_span.start, move_span.end
                        ),
                    ));
                    None
                }
                Some(OwnershipState::BorrowedMut(borrow_id)) => {
                    if let Some(info) = self.borrows.get(&borrow_id) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot borrow '{}' as mutable more than once (already borrowed at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                    None
                }
                Some(OwnershipState::Borrowed(ref borrow_ids)) if !borrow_ids.is_empty() => {
                    if let Some(info) = borrow_ids.first().and_then(|id| self.borrows.get(id)) {
                        self.errors.push(CompileError::borrow(
                            span,
                            format!(
                                "cannot borrow '{}' as mutable because it is already borrowed as immutable (at {}..{})",
                                name, info.span.start, info.span.end
                            ),
                        ));
                    }
                    None
                }
                Some(OwnershipState::Owned) | Some(OwnershipState::Borrowed(_)) | None => {
                    let borrow_id = self.fresh_borrow_id();
                    let scope_id = self.current_scope().id;
                    self.borrows.insert(
                        borrow_id,
                        BorrowInfo {
                            kind: BorrowKind::Mutable,
                            borrowed_from: binding_id,
                            span,
                            scope: scope_id,
                        },
                    );
                    self.ownership
                        .insert(binding_id, OwnershipState::BorrowedMut(borrow_id));
                    self.current_scope_mut().borrows.push(borrow_id);
                    Some(borrow_id)
                }
            }
        } else {
            None
        }
    }

    /// End a borrow
    fn end_borrow(&mut self, borrow_id: BorrowId) {
        if let Some(info) = self.borrows.remove(&borrow_id) {
            // Restore ownership state
            let binding_id = info.borrowed_from;
            match self.ownership.get(&binding_id).cloned() {
                Some(OwnershipState::Borrowed(mut borrow_ids)) => {
                    borrow_ids.retain(|id| *id != borrow_id);
                    if borrow_ids.is_empty() {
                        self.ownership.insert(binding_id, OwnershipState::Owned);
                    } else {
                        self.ownership
                            .insert(binding_id, OwnershipState::Borrowed(borrow_ids));
                    }
                }
                Some(OwnershipState::BorrowedMut(_)) => {
                    self.ownership.insert(binding_id, OwnershipState::Owned);
                }
                _ => {}
            }
        }
    }

    /// Check a program
    pub fn check(mut self, program: &Program) -> Result<(), Vec<CompileError>> {
        for item in &program.items {
            self.check_item(item);
        }
        self.errors.into_result(())
    }

    fn check_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.check_defun(defun),
            Item::Def(def) => self.check_def(def),
            Item::Defstruct(_defstruct) => {
                // Struct definitions don't have ownership semantics
            }
            Item::Defprotocol(_defprotocol) => {
                // Protocol definitions don't have ownership semantics
            }
            Item::ExtendProtocol(extend) => self.check_extend_protocol(extend),
        }
    }

    fn check_defun(&mut self, defun: &Defun) {
        self.push_scope();

        // Define parameters
        for param in &defun.params {
            self.define(&param.name.node, param.name.span);
        }

        // Check body
        self.check_expr(&defun.body);

        self.pop_scope();
    }

    fn check_def(&mut self, def: &Def) {
        // Check the value expression
        self.check_expr(&def.value);
        // Define the constant
        self.define(&def.name.node, def.name.span);
    }

    fn check_extend_protocol(&mut self, extend: &ExtendProtocol) {
        // Check each method implementation
        for method in &extend.implementations {
            self.push_scope();
            // Define parameters including self
            for param in &method.params {
                self.define(&param.node, param.span);
            }
            self.check_expr(&method.body);
            self.pop_scope();
        }
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::Nil => {
                // Literals are always OK
            }

            Expr::Var(name) => {
                // Using a variable - check ownership
                self.use_value(name, expr.span);
            }

            Expr::Call(func, args) => {
                // Check function
                self.check_expr(func);
                // Check args - they may move values
                for arg in args {
                    self.check_expr(arg);
                    // If arg is a simple variable, it might be moved
                    if let Expr::Var(name) = &arg.node {
                        // For now, we don't move on function calls
                        // (pass by reference optimization per ADR-002)
                        self.use_value(name, arg.span);
                    }
                }
            }

            Expr::Lambda(params, body) => {
                self.push_scope();
                for param in params {
                    self.define(&param.name.node, param.name.span);
                }
                self.check_expr(body);
                self.pop_scope();
            }

            Expr::Let(bindings, body) => {
                self.push_scope();
                for binding in bindings {
                    self.check_let_binding(binding);
                }
                self.check_expr(body);
                self.pop_scope();
            }

            Expr::Plet(bindings, body) => {
                // Same as let for ownership checking
                self.push_scope();
                for binding in bindings {
                    self.check_let_binding(binding);
                }
                self.check_expr(body);
                self.pop_scope();
            }

            Expr::If(cond, then_, else_) => {
                self.check_expr(cond);
                // Both branches need to be checked independently
                // (moves in one branch don't affect the other)
                // Save state before branches
                let saved_ownership = self.ownership.clone();
                let saved_borrows = self.borrows.clone();

                // Check then branch
                self.check_expr(then_);

                // Restore and check else branch
                let then_ownership = std::mem::replace(&mut self.ownership, saved_ownership);
                let then_borrows = std::mem::replace(&mut self.borrows, saved_borrows);
                self.check_expr(else_);

                // Merge: a value is moved if moved in BOTH branches
                // For now, take the more conservative approach - moved in either
                // TODO: proper merge for more precise analysis
                for (id, state) in then_ownership {
                    if let OwnershipState::Moved(span) = state {
                        if let Some(OwnershipState::Moved(_)) = self.ownership.get(&id) {
                            // Moved in both - keep the move
                        } else {
                            // Moved in then only - for now, mark as moved
                            // A more sophisticated analysis could track this
                            self.ownership.insert(id, OwnershipState::Moved(span));
                        }
                    }
                }
                // Keep borrows from else branch (they're the "latest")
                // Borrows should end when their scope ends anyway
                let _ = then_borrows;
            }

            Expr::Do(exprs) => {
                for expr in exprs {
                    self.check_expr(expr);
                }
            }

            Expr::Set(name, value) => {
                // Setting requires mutable borrow semantics
                self.check_expr(value);
                // The target must be owned and not borrowed
                self.use_value(&name.node, name.span);
            }

            Expr::Ref(inner) => {
                // Create a shared borrow
                if let Expr::Var(name) = &inner.node {
                    self.borrow_shared(name, expr.span);
                    // Don't also check inner as a use - the borrow IS the use
                } else {
                    self.check_expr(inner);
                }
            }

            Expr::RefMut(inner) => {
                // Create a mutable borrow
                if let Expr::Var(name) = &inner.node {
                    self.borrow_mut(name, expr.span);
                    // Don't also check inner as a use - the borrow IS the use
                } else {
                    self.check_expr(inner);
                }
            }

            Expr::Deref(inner) => {
                self.check_expr(inner);
            }

            Expr::Struct(_name, fields) => {
                for (_, value) in fields {
                    self.check_expr(value);
                }
            }

            Expr::Field(obj, _field) => {
                self.check_expr(obj);
            }

            Expr::Match(scrutinee, arms) => {
                self.check_expr(scrutinee);
                // If scrutinee is a variable, it might be moved
                if let Expr::Var(name) = &scrutinee.node {
                    self.move_value(name, scrutinee.span);
                }
                for arm in arms {
                    self.check_match_arm(arm);
                }
            }

            Expr::Quote(_) => {
                // Quoted symbols are always OK
            }

            Expr::Unsafe(inner) => {
                // Unsafe blocks still check ownership
                // (per ADR: same ownership rules apply inside)
                self.check_expr(inner);
            }

            // Atom expressions (ADR-011)
            Expr::Atom(value) => {
                // Creating an atom moves the value into the atom
                self.check_expr(value);
                if let Expr::Var(name) = &value.node {
                    self.move_value(name, value.span);
                }
            }
            Expr::AtomDeref(atom) => {
                // Reading an atom doesn't move it (atomic read)
                self.check_expr(atom);
            }
            Expr::Reset(atom, value) => {
                // Reset requires ownership of the new value
                self.check_expr(atom);
                self.check_expr(value);
                if let Expr::Var(name) = &value.node {
                    self.move_value(name, value.span);
                }
            }
            Expr::Swap(atom, func) => {
                // Swap uses the atom and the function
                self.check_expr(atom);
                self.check_expr(func);
            }
            Expr::CompareAndSet { atom, old, new } => {
                // CAS checks old value, potentially moves new value
                self.check_expr(atom);
                self.check_expr(old);
                self.check_expr(new);
            }

            // Persistent collections (ADR-018)
            Expr::Vector(elements) => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }
            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.check_expr(k);
                    self.check_expr(v);
                }
            }
            Expr::Keyword(_) => {}

            // Conventional mutable collections (ADR-018)
            Expr::ConvVector(elements) => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }
            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.check_expr(k);
                    self.check_expr(v);
                }
            }

            // Async/await (ADR-014)
            Expr::Async(body) => {
                self.check_expr(body);
            }
            Expr::Await(future) => {
                self.check_expr(future);
            }

            // SIMD vectors (ADR-016)
            Expr::SimdVector(elements) => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }

            // STM (ADR-012)
            Expr::Dosync(exprs) => {
                for expr in exprs {
                    self.check_expr(expr);
                }
            }
            Expr::RefSetStm(ref_expr, value) => {
                self.check_expr(ref_expr);
                self.check_expr(value);
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.check_expr(ref_expr);
                self.check_expr(fn_expr);
                for arg in args {
                    self.check_expr(arg);
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.check_expr(ref_expr);
                self.check_expr(fn_expr);
                for arg in args {
                    self.check_expr(arg);
                }
            }

            // Iterators - consume the collection/iterator
            Expr::Iter(coll) => {
                self.check_expr(coll);
                // iter consumes the collection
                if let Expr::Var(name) = &coll.node {
                    self.move_value(name, coll.span);
                }
            }
            Expr::Collect(iter) => {
                self.check_expr(iter);
                // collect consumes the iterator
                if let Expr::Var(name) = &iter.node {
                    self.move_value(name, iter.span);
                }
            }

            // Byte arrays and regex are literals - no ownership concerns
            Expr::ByteArray(_) | Expr::Regex { .. } => {}

            // Overflow handling - recurse into inner expression
            Expr::Boxed(inner) | Expr::Wrapping(inner) => {
                self.check_expr(inner);
            }
        }
    }

    fn check_let_binding(&mut self, binding: &LetBinding) {
        // Check the value
        self.check_expr(&binding.value);

        // If binding value is a variable, it's moved
        if let Expr::Var(name) = &binding.value.node {
            self.move_value(name, binding.value.span);
        }

        // Define the new binding
        self.define(&binding.name.node, binding.name.span);
    }

    fn check_match_arm(&mut self, arm: &MatchArm) {
        self.push_scope();
        // Pattern bindings are defined in this scope
        self.define_pattern_bindings(&arm.pattern);
        self.check_expr(&arm.body);
        self.pop_scope();
    }

    fn define_pattern_bindings(&mut self, pattern: &Spanned<crate::ast::Pattern>) {
        use crate::ast::Pattern;
        match &pattern.node {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                self.define(name, pattern.span);
            }
            Pattern::Literal(_) => {}
            Pattern::Struct(_, fields) => {
                for (_, pat) in fields {
                    self.define_pattern_bindings(&Spanned::new(pat.clone(), pattern.span));
                }
            }
            Pattern::Tuple(patterns) => {
                for pat in patterns {
                    self.define_pattern_bindings(&Spanned::new(pat.clone(), pattern.span));
                }
            }
        }
    }
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Check ownership and borrowing rules
pub fn check(program: &Program) -> crate::error::Result<()> {
    let checker = BorrowChecker::new();
    match checker.check(program) {
        Ok(()) => Ok(()),
        Err(errors) => Err(errors.into_iter().next().expect("at least one error")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn check_source(source: &str) -> Result<(), Vec<CompileError>> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        BorrowChecker::new().check(&program)
    }

    #[test]
    fn test_simple_use() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                x))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_use_after_move() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((y x))
                  x)))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("moved"));
    }

    #[test]
    fn test_double_move() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((y x))
                  (let ((z x))
                    z))))
            "#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_borrow_shared() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r (ref x)))
                  r)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_shared_borrows() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r1 (ref x)))
                  (let ((r2 (ref x)))
                    r1))))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_mutable_borrow_exclusive() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r1 (ref-mut x)))
                  (let ((r2 (ref-mut x)))
                    r1))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("mutable more than once"));
    }

    #[test]
    fn test_shared_and_mutable_conflict() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r1 (ref x)))
                  (let ((r2 (ref-mut x)))
                    r1))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("already borrowed as immutable"));
    }

    #[test]
    fn test_mutable_then_shared_conflict() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r1 (ref-mut x)))
                  (let ((r2 (ref x)))
                    r1))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("already borrowed as mutable"));
    }

    #[test]
    fn test_move_while_borrowed() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((r (ref x)))
                  (let ((y x))
                    y))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("while borrowed"));
    }

    #[test]
    fn test_if_branches_independent() {
        // Moves in different branches should be OK
        // (only one branch executes at runtime)
        let result = check_source(
            r#"
            (defun foo (cond)
              (let ((x 1))
                (if cond
                  (let ((y x)) y)
                  (let ((z x)) z))))
            "#,
        );
        // This is actually OK because moves happen in different branches
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_params_owned() {
        let result = check_source(
            r#"
            (defun foo (x)
              x)
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_lambda_captures() {
        let result = check_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) x)))
                  f)))
            "#,
        );
        assert!(result.is_ok());
    }
}
