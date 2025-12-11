//! Thread safety checking for closures
//!
//! Verifies that closures used in plet and async contexts are thread-safe.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Item, Program};
use crate::error::{CompileError, Errors};
use crate::span::{Span, Spanned};

use super::types::CaptureInfo;

/// Thread safety checker for plet and async contexts
pub struct ThreadSafetyChecker<'a> {
    /// Closure info from the analyzer
    closure_info: &'a HashMap<Span, CaptureInfo>,
    /// Collected errors
    errors: Errors,
    /// Whether we're currently in a plet context
    in_plet: bool,
    /// Bindings that are atoms (set! is allowed on these in plet)
    atom_bindings: HashSet<String>,
}

impl<'a> ThreadSafetyChecker<'a> {
    pub fn new(closure_info: &'a HashMap<Span, CaptureInfo>) -> Self {
        Self {
            closure_info,
            errors: Errors::new(),
            in_plet: false,
            atom_bindings: HashSet::new(),
        }
    }

    /// Check thread safety for a program
    pub fn check(mut self, program: &Program) -> std::result::Result<(), Vec<CompileError>> {
        for item in &program.items {
            self.check_item(item);
        }
        self.errors.into_result(())
    }

    /// Check if an expression creates an atom
    fn is_atom_expr(expr: &Expr) -> bool {
        matches!(expr, Expr::Atom(_))
    }

    /// Check if a function name is a parallel operation that requires Sync closures
    fn is_parallel_op(name: &str) -> bool {
        matches!(name, "pmap" | "pfilter" | "preduce" | "pfor" | "spawn")
    }

    fn check_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.check_expr(&defun.body),
            Item::Def(def) => self.check_expr(&def.value),
            Item::Defstruct(_) => {}
            Item::Defprotocol(_) => {}
            Item::ExtendProtocol(extend) => {
                for method in &extend.implementations {
                    self.check_expr(&method.body);
                }
            }
            Item::Defmacro(_) => {}
            Item::Extern(_) => {}
        }
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Lambda(_, body) => {
                // Check if this lambda is used in a plet context
                if self.in_plet {
                    if let Some(info) = self.closure_info.get(&expr.span) {
                        if !info.color.is_thread_safe() {
                            self.errors.push(CompileError::thread_safety(
                                expr.span,
                                format!(
                                    "closure used in plet {}",
                                    info.color.thread_safety_reason()
                                ),
                            ));
                        }
                    }
                }
                self.check_expr(body);
            }

            Expr::Plet(bindings, body) => {
                let was_in_plet = self.in_plet;
                let saved_atom_bindings = self.atom_bindings.clone();
                self.in_plet = true;

                // Track which bindings are atoms
                for binding in bindings {
                    if Self::is_atom_expr(&binding.value.node) {
                        self.atom_bindings.insert(binding.name.node.clone());
                    }
                    self.check_expr(&binding.value);
                }
                self.check_expr(body);

                self.in_plet = was_in_plet;
                self.atom_bindings = saved_atom_bindings;
            }

            Expr::Let(bindings, body) => {
                for binding in bindings {
                    self.check_expr(&binding.value);
                }
                self.check_expr(body);
            }

            Expr::Call(func, args) => {
                // Check if this is a parallel operation (pmap, pfilter, etc.)
                if let Expr::Var(name) = &func.node {
                    if Self::is_parallel_op(name) {
                        // First argument should be a Sync/Pure closure
                        if let Some(fn_arg) = args.first() {
                            if let Expr::Lambda(_, _) = &fn_arg.node {
                                if let Some(info) = self.closure_info.get(&fn_arg.span) {
                                    if !info.color.is_thread_safe() {
                                        self.errors.push(CompileError::thread_safety(
                                            fn_arg.span,
                                            format!(
                                                "{} requires a thread-safe closure, but this closure {}",
                                                name,
                                                info.color.thread_safety_reason()
                                            ),
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
                self.check_expr(func);
                for arg in args {
                    self.check_expr(arg);
                }
            }

            Expr::If(cond, then_, else_) => {
                self.check_expr(cond);
                self.check_expr(then_);
                self.check_expr(else_);
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.check_expr(e);
                }
            }

            Expr::Set(name, value) => {
                // In plet context, only atom bindings can be mutated
                if self.in_plet && !self.atom_bindings.contains(&name.node) {
                    self.errors.push(CompileError::thread_safety(
                        name.span,
                        format!(
                            "cannot mutate non-atom binding '{}' in plet; use (atom value) for mutable state",
                            name.node
                        ),
                    ));
                }
                self.check_expr(value);
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
                self.check_expr(inner);
            }

            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    self.check_expr(value);
                }
            }

            Expr::Field(obj, _) => {
                self.check_expr(obj);
            }

            // Atom expressions
            Expr::Atom(value) => {
                self.check_expr(value);
            }
            Expr::Swap(atom, func) => {
                self.check_expr(atom);
                self.check_expr(func);
            }
            Expr::Reset(atom, value) => {
                self.check_expr(atom);
                self.check_expr(value);
            }
            Expr::AtomDeref(atom) => {
                self.check_expr(atom);
            }
            Expr::CompareAndSet { atom, old, new } => {
                self.check_expr(atom);
                self.check_expr(old);
                self.check_expr(new);
            }

            // Persistent collections
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

            // Conventional mutable collections
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

            // Async/await - async blocks require thread-safe closures
            Expr::Async(body) => {
                // Check if body contains closures that aren't thread-safe
                // Async blocks have similar requirements to plet
                let was_in_plet = self.in_plet;
                self.in_plet = true; // Async has same thread-safety requirements
                self.check_expr(body);
                self.in_plet = was_in_plet;
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

            // Iterators
            Expr::Iter(coll) => {
                self.check_expr(coll);
            }
            Expr::Collect(iter) => {
                self.check_expr(iter);
            }

            // Literals and simple expressions
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Quote(_)
            | Expr::Keyword(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. } => {}

            // Overflow handling - recurse into inner expression
            Expr::Boxed(inner) | Expr::Wrapping(inner) => {
                self.check_expr(inner);
            }

            // Macro syntax should be expanded before this check
            Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
                self.check_expr(inner);
            }
            Expr::Gensym(_) => {}

            // Generated by closure conversion pass (after analysis)
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.check_expr(e);
                }
            }
            Expr::RcAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.check_expr(value);
                }
            }
        }
    }
}
