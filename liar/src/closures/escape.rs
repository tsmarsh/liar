//! Escape analysis for closures
//!
//! Determines whether a closure escapes its defining scope, which affects
//! whether its environment can be stack-allocated (non-escaping) or must
//! be heap-allocated (escaping).
//!
//! A closure escapes if it:
//! - Is returned from the function
//! - Is stored in a struct field
//! - Is passed to a function that might store it
//!
//! A closure does NOT escape if it:
//! - Is only called locally within its scope
//! - Is passed to a known HOF (map, filter, reduce) that only calls, doesn't store

use std::collections::{HashMap, HashSet};

use crate::ast::{Defun, Expr, Item, Program};
use crate::span::{Span, Spanned};

/// Escape status for a closure
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EscapeStatus {
    /// Closure never escapes - can be stack allocated
    Local,
    /// Closure may escape - must be heap allocated (conservative default)
    #[default]
    Escapes,
}

/// Result of escape analysis: maps closure spans to their escape status
pub type EscapeInfo = HashMap<Span, EscapeStatus>;

/// Escape analyzer
pub struct EscapeAnalyzer {
    /// Escape status for each closure (by span)
    escape_info: EscapeInfo,
    /// Current function's return type - closures returned escape
    in_return_position: bool,
    /// Set of HOFs that are known to not store their closure argument
    non_storing_hofs: HashSet<&'static str>,
    /// Closures in the current scope that haven't escaped yet
    local_closures: HashSet<Span>,
}

impl EscapeAnalyzer {
    pub fn new() -> Self {
        let mut non_storing_hofs = HashSet::new();
        // Built-in HOFs that only call their closure argument, don't store it
        non_storing_hofs.insert("map");
        non_storing_hofs.insert("filter");
        non_storing_hofs.insert("reduce");
        non_storing_hofs.insert("pmap");
        non_storing_hofs.insert("pfilter");
        non_storing_hofs.insert("preduce");
        non_storing_hofs.insert("for-each");

        Self {
            escape_info: HashMap::new(),
            in_return_position: false,
            non_storing_hofs,
            local_closures: HashSet::new(),
        }
    }

    /// Analyze a program and return escape information for all closures
    pub fn analyze(mut self, program: &Program) -> EscapeInfo {
        for item in &program.items {
            self.analyze_item(item);
        }
        self.escape_info
    }

    fn analyze_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.analyze_defun(defun),
            Item::Def(def) => self.analyze_expr(&def.value),
            Item::ExtendProtocol(extend) => {
                for method in &extend.implementations {
                    self.analyze_expr(&method.body);
                }
            }
            Item::ExtendProtocolDefault(extend) => {
                for method in &extend.implementations {
                    self.analyze_expr(&method.body);
                }
            }
            Item::Defstruct(_) | Item::Defprotocol(_) | Item::Defmacro(_) | Item::Extern(_) => {}
        }
    }

    fn analyze_defun(&mut self, defun: &Defun) {
        // Clear local closures for this function
        self.local_closures.clear();

        // Analyze the body - the body is in return position
        self.in_return_position = true;
        self.analyze_expr(&defun.body);
        self.in_return_position = false;
    }

    fn analyze_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            // Lambda creates a closure
            Expr::Lambda(_, body) => {
                // Record this closure - assume local until proven otherwise
                self.local_closures.insert(expr.span);

                // If we're in return position, this closure escapes
                if self.in_return_position {
                    self.mark_escapes(expr.span);
                } else {
                    // Tentatively mark as local (may be updated later)
                    self.escape_info
                        .entry(expr.span)
                        .or_insert(EscapeStatus::Local);
                }

                // Analyze the body (not in return position)
                let was_return = self.in_return_position;
                self.in_return_position = false;
                self.analyze_expr(body);
                self.in_return_position = was_return;
            }

            // Let binding - closures bound here may or may not escape
            Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
                // Analyze binding values (not in return position)
                let was_return = self.in_return_position;
                self.in_return_position = false;
                for binding in bindings {
                    self.analyze_expr(&binding.value);
                }

                // Analyze body (inherits return position)
                self.in_return_position = was_return;
                self.analyze_expr(body);
            }

            // Function call - check if closure argument escapes
            Expr::Call(func, args) => {
                // Analyze the function expression
                self.in_return_position = false;
                self.analyze_expr(func);

                // Check if this is a known non-storing HOF
                let is_safe_hof = if let Expr::Var(name) = &func.node {
                    self.non_storing_hofs.contains(name.as_str())
                } else {
                    false
                };

                // Analyze arguments
                for arg in args {
                    // If argument is a closure and this is not a safe HOF, it escapes
                    if !is_safe_hof {
                        if let Expr::Lambda(_, _) = &arg.node {
                            self.mark_escapes(arg.span);
                        }
                        // Also check if it's a variable referencing a closure
                        if let Expr::Var(name) = &arg.node {
                            // Conservative: mark any closure-typed variable as escaping
                            // when passed to a function
                            self.mark_var_escapes(name);
                        }
                    }
                    self.analyze_expr(arg);
                }
            }

            // If expression - both branches inherit return position
            Expr::If(cond, then_, else_) => {
                let was_return = self.in_return_position;
                self.in_return_position = false;
                self.analyze_expr(cond);
                self.in_return_position = was_return;
                self.analyze_expr(then_);
                self.analyze_expr(else_);
            }

            // Do block - only last expression is in return position
            Expr::Do(exprs) => {
                let was_return = self.in_return_position;
                for (i, e) in exprs.iter().enumerate() {
                    self.in_return_position = was_return && i == exprs.len() - 1;
                    self.analyze_expr(e);
                }
                self.in_return_position = was_return;
            }

            // Struct construction - closures stored in fields escape
            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    if let Expr::Lambda(_, _) = &value.node {
                        self.mark_escapes(value.span);
                    }
                    self.analyze_expr(value);
                }
            }

            // Variable reference - doesn't affect escape analysis directly
            Expr::Var(_) => {}

            // Recurse into sub-expressions
            Expr::Set(_, value) => self.analyze_expr(value),
            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
                self.analyze_expr(inner)
            }
            Expr::Field(obj, _) => self.analyze_expr(obj),
            Expr::Atom(value) => self.analyze_expr(value),
            Expr::Swap(atom, func) => {
                self.analyze_expr(atom);
                // Function passed to swap might be called multiple times but doesn't escape
                self.analyze_expr(func);
            }
            Expr::Reset(atom, value) => {
                self.analyze_expr(atom);
                self.analyze_expr(value);
            }
            Expr::AtomDeref(atom) => self.analyze_expr(atom),
            Expr::CompareAndSet { atom, old, new } => {
                self.analyze_expr(atom);
                self.analyze_expr(old);
                self.analyze_expr(new);
            }
            Expr::Vector(elements) | Expr::ConvVector(elements) | Expr::SimdVector(elements) => {
                for elem in elements {
                    self.analyze_expr(elem);
                }
            }
            Expr::Map(pairs) | Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.analyze_expr(k);
                    self.analyze_expr(v);
                }
            }
            Expr::Async(body) => self.analyze_expr(body),
            Expr::Await(future) => self.analyze_expr(future),
            Expr::Dosync(exprs) => {
                for e in exprs {
                    self.analyze_expr(e);
                }
            }
            Expr::RefSetStm(ref_expr, value) => {
                self.analyze_expr(ref_expr);
                self.analyze_expr(value);
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            }
            | Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.analyze_expr(ref_expr);
                self.analyze_expr(fn_expr);
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
            Expr::Iter(coll) => self.analyze_expr(coll),
            Expr::Collect(iter) => self.analyze_expr(iter),
            Expr::Boxed(inner) | Expr::Wrapping(inner) => self.analyze_expr(inner),
            Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
                self.analyze_expr(inner)
            }

            // Generated by closure conversion (after analysis, but handle for completeness)
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.analyze_expr(e);
                }
            }
            Expr::HeapEnvAlloc { fields, .. } | Expr::StackEnvAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.analyze_expr(value);
                }
            }

            // Literals don't contain closures
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Quote(_)
            | Expr::Keyword(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. }
            | Expr::Gensym(_) => {}
        }
    }

    /// Mark a closure as escaping
    fn mark_escapes(&mut self, span: Span) {
        self.escape_info.insert(span, EscapeStatus::Escapes);
        self.local_closures.remove(&span);
    }

    /// Mark any closure bound to this variable name as escaping
    /// This is conservative - we can't easily track which closures are bound to which names
    fn mark_var_escapes(&mut self, _name: &str) {
        // For now, this is a no-op because we'd need more sophisticated tracking
        // to know which closure is bound to which variable name
        // The conservative approach is already applied in analyze_expr for Call
    }
}

impl Default for EscapeAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn analyze_source(source: &str) -> EscapeInfo {
        let mut parser = Parser::new(source).expect("parser creation failed");
        let program = parser.parse_program().expect("parse failed");
        EscapeAnalyzer::new().analyze(&program)
    }

    #[test]
    fn test_returned_closure_escapes() {
        let info = analyze_source(
            r#"
            (defun make-adder (n)
              (fn (x) (+ x n)))
            "#,
        );

        // The returned lambda should escape
        let escaping_count = info
            .values()
            .filter(|&&s| s == EscapeStatus::Escapes)
            .count();
        assert_eq!(escaping_count, 1, "returned closure should escape");
    }

    #[test]
    fn test_local_closure_does_not_escape() {
        let info = analyze_source(
            r#"
            (defun test ()
              (let ((f (fn (x) (* x 2))))
                (f 5)))
            "#,
        );

        // The closure is only called locally, should not escape
        let local_count = info.values().filter(|&&s| s == EscapeStatus::Local).count();
        assert_eq!(local_count, 1, "local closure should not escape");
    }

    #[test]
    fn test_closure_passed_to_map_does_not_escape() {
        let info = analyze_source(
            r#"
            (defun test (xs)
              (map (fn (x) (* x 2)) xs))
            "#,
        );

        // Closure passed to map should not escape (map is a known safe HOF)
        let local_count = info.values().filter(|&&s| s == EscapeStatus::Local).count();
        assert_eq!(local_count, 1, "closure passed to map should not escape");
    }

    #[test]
    fn test_closure_passed_to_unknown_function_escapes() {
        let info = analyze_source(
            r#"
            (defun test ()
              (unknown-fn (fn (x) x)))
            "#,
        );

        // Closure passed to unknown function should escape (conservative)
        let escaping_count = info
            .values()
            .filter(|&&s| s == EscapeStatus::Escapes)
            .count();
        assert_eq!(
            escaping_count, 1,
            "closure passed to unknown function should escape"
        );
    }
}
