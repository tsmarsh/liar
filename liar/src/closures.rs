//! Closure analysis
//!
//! Determines what variables closures capture and how (by value, by ref, by mut ref).
//! Also determines closure "color" (single-threaded vs thread-safe) per ADR-010.

use std::collections::{HashMap, HashSet};

use crate::ast::{Def, Defun, Expr, Item, LetBinding, MatchArm, Param, Pattern, Program};
use crate::error::{CompileError, Errors, Result};
use crate::resolve::BindingId;
use crate::span::{Span, Spanned};

/// Closure capture mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    /// Capture by value (move)
    Move,
    /// Capture by immutable borrow (closure has lifetime bound)
    Borrow,
    /// Capture by clone (for Copy types or explicit clone)
    Clone,
}

/// Closure "color" for thread safety (ADR-010)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ClosureColor {
    /// No captures, can go anywhere
    #[default]
    Pure,
    /// Captures borrows, cannot escape scope
    Local,
    /// Captures only Send+Sync values, thread-safe
    Sync,
    /// Captures non-Send values, single-threaded only
    NonSync,
}

impl ClosureColor {
    /// Combine two colors (most restrictive wins)
    pub fn combine(self, other: ClosureColor) -> ClosureColor {
        match (self, other) {
            (ClosureColor::Pure, x) | (x, ClosureColor::Pure) => x,
            (ClosureColor::Local, _) | (_, ClosureColor::Local) => ClosureColor::Local,
            (ClosureColor::NonSync, _) | (_, ClosureColor::NonSync) => ClosureColor::NonSync,
            (ClosureColor::Sync, ClosureColor::Sync) => ClosureColor::Sync,
        }
    }

    /// Check if this color is thread-safe (can be used in plet, async, spawn)
    pub fn is_thread_safe(&self) -> bool {
        matches!(self, ClosureColor::Pure | ClosureColor::Sync)
    }

    /// Get a human-readable description of why this color isn't thread-safe
    pub fn thread_safety_reason(&self) -> &'static str {
        match self {
            ClosureColor::Pure => "is thread-safe (no captures)",
            ClosureColor::Sync => "is thread-safe (captures only Send+Sync values)",
            ClosureColor::Local => "captures borrowed references and cannot be used across threads",
            ClosureColor::NonSync => "captures non-Send values and cannot be used across threads",
        }
    }
}

/// Information about a single capture
#[derive(Debug, Clone)]
pub struct Capture {
    /// Name of the captured variable
    pub name: String,
    /// Binding ID (if resolved)
    pub binding: Option<BindingId>,
    /// How the variable is captured
    pub mode: CaptureMode,
    /// Where the capture occurs
    pub span: Span,
}

/// Information about a closure's captures
#[derive(Debug, Clone, Default)]
pub struct CaptureInfo {
    /// All captured variables
    pub captures: Vec<Capture>,
    /// The closure's color
    pub color: ClosureColor,
}

/// Closure analyzer state
pub struct ClosureAnalyzer {
    /// Variables in scope at each level
    scope_stack: Vec<HashSet<String>>,
    /// Capture info for each lambda (keyed by span)
    pub closure_info: HashMap<Span, CaptureInfo>,
    /// Next binding ID for internal tracking
    next_id: u32,
}

impl ClosureAnalyzer {
    pub fn new() -> Self {
        Self {
            scope_stack: vec![HashSet::new()],
            closure_info: HashMap::new(),
            next_id: 0,
        }
    }

    fn fresh_id(&mut self) -> BindingId {
        let id = BindingId(self.next_id);
        self.next_id += 1;
        id
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name.to_string());
        }
    }

    /// Analyze a program for closures
    pub fn analyze(mut self, program: &Program) -> Result<HashMap<Span, CaptureInfo>> {
        for item in &program.items {
            self.analyze_item(item);
        }
        Ok(self.closure_info)
    }

    fn analyze_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.analyze_defun(defun),
            Item::Def(def) => self.analyze_def(def),
            Item::Defstruct(_) => {}
        }
    }

    fn analyze_defun(&mut self, defun: &Defun) {
        self.push_scope();

        // Define function name (for recursion)
        self.define(&defun.name.node);

        // Define parameters
        for param in &defun.params {
            self.define(&param.name.node);
        }

        // Analyze body
        self.analyze_expr(&defun.body);

        self.pop_scope();
    }

    fn analyze_def(&mut self, def: &Def) {
        self.define(&def.name.node);
        self.analyze_expr(&def.value);
    }

    fn analyze_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::Nil => {}

            Expr::Var(_) => {
                // Variable use - nothing to analyze for closures at this level
            }

            Expr::Call(func, args) => {
                self.analyze_expr(func);
                for arg in args {
                    self.analyze_expr(arg);
                }
            }

            Expr::Lambda(params, body) => {
                // This is a closure! Analyze its captures.
                let captures = self.analyze_lambda(params, body, expr.span);
                self.closure_info.insert(expr.span, captures);
            }

            Expr::Let(bindings, body) => {
                self.push_scope();
                for binding in bindings {
                    self.analyze_let_binding(binding);
                }
                self.analyze_expr(body);
                self.pop_scope();
            }

            Expr::Plet(bindings, body) => {
                self.push_scope();
                for binding in bindings {
                    self.analyze_let_binding(binding);
                }
                self.analyze_expr(body);
                self.pop_scope();
            }

            Expr::If(cond, then_, else_) => {
                self.analyze_expr(cond);
                self.analyze_expr(then_);
                self.analyze_expr(else_);
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.analyze_expr(e);
                }
            }

            Expr::Set(_, value) => {
                self.analyze_expr(value);
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
                self.analyze_expr(inner);
            }

            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    self.analyze_expr(value);
                }
            }

            Expr::Field(obj, _) => {
                self.analyze_expr(obj);
            }

            Expr::Match(scrutinee, arms) => {
                self.analyze_expr(scrutinee);
                for arm in arms {
                    self.analyze_match_arm(arm);
                }
            }

            Expr::Quote(_) => {}
        }
    }

    fn analyze_let_binding(&mut self, binding: &LetBinding) {
        self.analyze_expr(&binding.value);
        self.define(&binding.name.node);
    }

    fn analyze_match_arm(&mut self, arm: &MatchArm) {
        self.push_scope();
        self.define_pattern_bindings(&arm.pattern);
        self.analyze_expr(&arm.body);
        self.pop_scope();
    }

    fn define_pattern_bindings(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.node {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                self.define(name);
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

    /// Analyze a lambda to determine its captures
    fn analyze_lambda(
        &mut self,
        params: &[Param],
        body: &Spanned<Expr>,
        _span: Span,
    ) -> CaptureInfo {
        // For finding captures, we need to know what's defined INSIDE the lambda
        // (parameters) vs what's used from OUTSIDE (captures)
        let mut lambda_local: HashSet<String> = HashSet::new();
        for param in params {
            lambda_local.insert(param.name.node.clone());
        }

        // Find free variables - variables used in body but not defined inside lambda
        let free_vars = self.find_free_vars_for_lambda(body, &lambda_local);

        // Determine capture mode for each free variable
        let captures: Vec<Capture> = free_vars
            .into_iter()
            .map(|(name, usage_span)| {
                let mode = self.determine_capture_mode(&name, body);
                Capture {
                    name,
                    binding: Some(self.fresh_id()),
                    mode,
                    span: usage_span,
                }
            })
            .collect();

        // Compute the closure's color
        let color = self.compute_color(&captures);

        // Recursively analyze the body for nested closures
        self.push_scope();
        for param in params {
            self.define(&param.name.node);
        }
        self.analyze_expr(body);
        self.pop_scope();

        CaptureInfo { captures, color }
    }

    /// Find free variables for a lambda (variables used but not defined inside the lambda)
    fn find_free_vars_for_lambda(
        &self,
        expr: &Spanned<Expr>,
        lambda_local: &HashSet<String>,
    ) -> Vec<(String, Span)> {
        let mut free = Vec::new();
        let mut local_scope = lambda_local.clone();
        self.find_free_vars_lambda_inner(expr, &mut local_scope, &mut free);
        free
    }

    fn find_free_vars_lambda_inner(
        &self,
        expr: &Spanned<Expr>,
        local: &mut HashSet<String>,
        free: &mut Vec<(String, Span)>,
    ) {
        match &expr.node {
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::Nil => {}

            Expr::Var(name) => {
                // If not defined inside the lambda and not a builtin, it's a capture
                if !local.contains(name)
                    && !self.is_builtin(name)
                    && !free.iter().any(|(n, _)| n == name)
                {
                    free.push((name.clone(), expr.span));
                }
            }

            Expr::Call(func, args) => {
                self.find_free_vars_lambda_inner(func, local, free);
                for arg in args {
                    self.find_free_vars_lambda_inner(arg, local, free);
                }
            }

            Expr::Lambda(params, body) => {
                // Create a nested scope for the inner lambda
                let mut inner_local = local.clone();
                for param in params {
                    inner_local.insert(param.name.node.clone());
                }
                self.find_free_vars_lambda_inner(body, &mut inner_local, free);
            }

            Expr::Let(bindings, body) => {
                for binding in bindings {
                    self.find_free_vars_lambda_inner(&binding.value, local, free);
                    local.insert(binding.name.node.clone());
                }
                self.find_free_vars_lambda_inner(body, local, free);
            }

            Expr::Plet(bindings, body) => {
                for binding in bindings {
                    self.find_free_vars_lambda_inner(&binding.value, local, free);
                    local.insert(binding.name.node.clone());
                }
                self.find_free_vars_lambda_inner(body, local, free);
            }

            Expr::If(cond, then_, else_) => {
                self.find_free_vars_lambda_inner(cond, local, free);
                self.find_free_vars_lambda_inner(then_, local, free);
                self.find_free_vars_lambda_inner(else_, local, free);
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.find_free_vars_lambda_inner(e, local, free);
                }
            }

            Expr::Set(name, value) => {
                if !local.contains(&name.node)
                    && !self.is_builtin(&name.node)
                    && !free.iter().any(|(n, _)| n == &name.node)
                {
                    free.push((name.node.clone(), name.span));
                }
                self.find_free_vars_lambda_inner(value, local, free);
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
                self.find_free_vars_lambda_inner(inner, local, free);
            }

            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    self.find_free_vars_lambda_inner(value, local, free);
                }
            }

            Expr::Field(obj, _) => {
                self.find_free_vars_lambda_inner(obj, local, free);
            }

            Expr::Match(scrutinee, arms) => {
                self.find_free_vars_lambda_inner(scrutinee, local, free);
                for arm in arms {
                    let mut arm_local = local.clone();
                    Self::add_pattern_bindings(&arm.pattern, &mut arm_local);
                    self.find_free_vars_lambda_inner(&arm.body, &mut arm_local, free);
                }
            }

            Expr::Quote(_) => {}
        }
    }

    fn add_pattern_bindings(pattern: &Spanned<Pattern>, local: &mut HashSet<String>) {
        match &pattern.node {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                local.insert(name.clone());
            }
            Pattern::Literal(_) => {}
            Pattern::Struct(_, fields) => {
                for (_, pat) in fields {
                    Self::add_pattern_bindings(&Spanned::new(pat.clone(), pattern.span), local);
                }
            }
            Pattern::Tuple(patterns) => {
                for pat in patterns {
                    Self::add_pattern_bindings(&Spanned::new(pat.clone(), pattern.span), local);
                }
            }
        }
    }

    /// Check if a name is a builtin function
    fn is_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "+" | "-"
                | "*"
                | "/"
                | "rem"
                | "="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "not"
                | "and"
                | "or"
                | "print"
                | "println"
                | "cons"
                | "car"
                | "cdr"
                | "list"
                | "nil?"
                | "empty?"
                | "int"
                | "float"
                | "string"
        )
    }

    /// Determine how a variable should be captured
    fn determine_capture_mode(&self, name: &str, body: &Spanned<Expr>) -> CaptureMode {
        // Check if the variable is used through a reference (either & or &mut)
        if Self::is_used_as_borrow(name, body) {
            CaptureMode::Borrow
        } else {
            // Default to move capture
            CaptureMode::Move
        }
    }

    /// Check if a variable is used through any kind of borrow (ref or ref-mut)
    fn is_used_as_borrow(name: &str, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Ref(inner) | Expr::RefMut(inner) => {
                if let Expr::Var(var_name) = &inner.node {
                    if var_name == name {
                        return true;
                    }
                }
                Self::is_used_as_borrow(name, inner)
            }
            Expr::Call(func, args) => {
                Self::is_used_as_borrow(name, func)
                    || args.iter().any(|a| Self::is_used_as_borrow(name, a))
            }
            Expr::Lambda(_, body) => Self::is_used_as_borrow(name, body),
            Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
                bindings
                    .iter()
                    .any(|b| Self::is_used_as_borrow(name, &b.value))
                    || Self::is_used_as_borrow(name, body)
            }
            Expr::If(c, t, e) => {
                Self::is_used_as_borrow(name, c)
                    || Self::is_used_as_borrow(name, t)
                    || Self::is_used_as_borrow(name, e)
            }
            Expr::Do(exprs) => exprs.iter().any(|e| Self::is_used_as_borrow(name, e)),
            Expr::Deref(inner) | Expr::Unsafe(inner) => Self::is_used_as_borrow(name, inner),
            _ => false,
        }
    }

    /// Compute the closure's color based on its captures
    fn compute_color(&self, captures: &[Capture]) -> ClosureColor {
        if captures.is_empty() {
            return ClosureColor::Pure;
        }

        let mut color = ClosureColor::Pure;

        for capture in captures {
            let capture_color = match capture.mode {
                CaptureMode::Borrow => ClosureColor::Local, // Borrows = can't escape
                CaptureMode::Move => ClosureColor::Sync,    // Moves = thread-safe (for now)
                CaptureMode::Clone => ClosureColor::Sync,   // Clones = thread-safe
            };
            color = color.combine(capture_color);
        }

        color
    }
}

impl Default for ClosureAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Analyze closures in a program
pub fn analyze(program: &mut Program) -> Result<()> {
    let analyzer = ClosureAnalyzer::new();
    let _info = analyzer.analyze(program)?;
    // The closure info would typically be stored somewhere for codegen
    // For now, we just validate that analysis succeeds
    Ok(())
}

/// Thread safety checker for plet and async contexts
pub struct ThreadSafetyChecker<'a> {
    /// Closure info from the analyzer
    closure_info: &'a HashMap<Span, CaptureInfo>,
    /// Collected errors
    errors: Errors,
    /// Whether we're currently in a plet context
    in_plet: bool,
}

impl<'a> ThreadSafetyChecker<'a> {
    pub fn new(closure_info: &'a HashMap<Span, CaptureInfo>) -> Self {
        Self {
            closure_info,
            errors: Errors::new(),
            in_plet: false,
        }
    }

    /// Check thread safety for a program
    pub fn check(mut self, program: &Program) -> std::result::Result<(), Vec<CompileError>> {
        for item in &program.items {
            self.check_item(item);
        }
        self.errors.into_result(())
    }

    fn check_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.check_expr(&defun.body),
            Item::Def(def) => self.check_expr(&def.value),
            Item::Defstruct(_) => {}
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
                self.in_plet = true;

                for binding in bindings {
                    self.check_expr(&binding.value);
                }
                self.check_expr(body);

                self.in_plet = was_in_plet;
            }

            Expr::Let(bindings, body) => {
                for binding in bindings {
                    self.check_expr(&binding.value);
                }
                self.check_expr(body);
            }

            Expr::Call(func, args) => {
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

            Expr::Set(_, value) => {
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

            Expr::Match(scrutinee, arms) => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_expr(&arm.body);
                }
            }

            // Literals and simple expressions
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Quote(_) => {}
        }
    }
}

/// Full closure analysis with thread safety checking
pub fn analyze_with_thread_safety(
    program: &Program,
) -> std::result::Result<HashMap<Span, CaptureInfo>, Vec<CompileError>> {
    let analyzer = ClosureAnalyzer::new();
    let info = analyzer.analyze(program).map_err(|e| vec![e])?;

    // Check thread safety
    let checker = ThreadSafetyChecker::new(&info);
    checker.check(program)?;

    Ok(info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn analyze_source(source: &str) -> HashMap<Span, CaptureInfo> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        ClosureAnalyzer::new().analyze(&program).unwrap()
    }

    #[test]
    fn test_no_captures() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) x)))
                f))
            "#,
        );
        // Should have one closure with no captures
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert!(capture_info.captures.is_empty());
        assert_eq!(capture_info.color, ClosureColor::Pure);
    }

    #[test]
    fn test_single_capture() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ x y))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].name, "x");
        assert_eq!(capture_info.captures[0].mode, CaptureMode::Move);
    }

    #[test]
    fn test_multiple_captures() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1) (y 2))
                (let ((f (fn (z) (+ x (+ y z)))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 2);
        let names: HashSet<_> = capture_info.captures.iter().map(|c| &c.name).collect();
        assert!(names.contains(&"x".to_string()));
        assert!(names.contains(&"y".to_string()));
    }

    #[test]
    fn test_borrow_capture() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ (deref (ref x)) y))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].mode, CaptureMode::Borrow);
        assert_eq!(capture_info.color, ClosureColor::Local);
    }

    #[test]
    fn test_nested_closure() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (fn (z) (+ x (+ y z))))))
                  f)))
            "#,
        );
        // Should have two closures
        assert_eq!(info.len(), 2);
    }

    #[test]
    fn test_closure_color_pure() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) (+ x 1))))
                f))
            "#,
        );
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.color, ClosureColor::Pure);
    }

    #[test]
    fn test_closure_color_local() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ (deref (ref x)) y))))
                  f)))
            "#,
        );
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.color, ClosureColor::Local);
    }

    #[test]
    fn test_does_not_capture_params() {
        let info = analyze_source(
            r#"
            (defun foo (x)
              (let ((f (fn (y) (+ x y))))
                f))
            "#,
        );
        // x is a parameter of foo, so it should be captured by the inner closure
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].name, "x");
    }

    #[test]
    fn test_builtin_not_captured() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x y) (+ x y))))
                f))
            "#,
        );
        // + is a builtin, should not be captured
        let capture_info = info.values().next().unwrap();
        assert!(capture_info.captures.is_empty());
    }

    fn check_thread_safety(source: &str) -> std::result::Result<(), Vec<CompileError>> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        analyze_with_thread_safety(&program).map(|_| ())
    }

    #[test]
    fn test_plet_pure_closure_ok() {
        // Pure closures (no captures) are OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((f (fn (x) (+ x 1))))
                (f 42)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_sync_closure_ok() {
        // Sync closures (move captures) are OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (let ((x 1))
                (plet ((f (fn (y) (+ x y))))
                  (f 42))))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_local_closure_error() {
        // Local closures (borrow captures) are NOT OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (let ((x 1))
                (plet ((f (fn (y) (+ (deref (ref x)) y))))
                  (f 42))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("plet"));
        assert!(errors[0].message.contains("borrowed"));
    }
}
