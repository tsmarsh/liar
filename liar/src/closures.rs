//! Closure analysis
//!
//! Determines what variables closures capture and how (by value, by ref, by mut ref).
//! Also determines closure "color" (single-threaded vs thread-safe) per ADR-010.

use std::collections::{HashMap, HashSet};

use crate::ast::{
    Def, Defun, Expr, ExtendProtocol, Item, LetBinding, MatchArm, Param, Pattern, Program,
};
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
            Item::Defprotocol(_) => {}
            Item::ExtendProtocol(extend) => self.analyze_extend_protocol(extend),
            // Macros should be expanded before closure analysis
            Item::Defmacro(_) => {}
            // Extern declarations have no body
            Item::Extern(_) => {}
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

    fn analyze_extend_protocol(&mut self, extend: &ExtendProtocol) {
        // Analyze each method implementation
        for method in &extend.implementations {
            self.push_scope();
            // Define parameters including self
            for param in &method.params {
                self.define(&param.node);
            }
            self.analyze_expr(&method.body);
            self.pop_scope();
        }
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

            // Atom expressions
            Expr::Atom(value) => {
                self.analyze_expr(value);
            }
            Expr::Swap(atom, func) => {
                self.analyze_expr(atom);
                self.analyze_expr(func);
            }
            Expr::Reset(atom, value) => {
                self.analyze_expr(atom);
                self.analyze_expr(value);
            }
            Expr::AtomDeref(atom) => {
                self.analyze_expr(atom);
            }
            Expr::CompareAndSet { atom, old, new } => {
                self.analyze_expr(atom);
                self.analyze_expr(old);
                self.analyze_expr(new);
            }

            // Persistent collections
            Expr::Vector(elements) => {
                for elem in elements {
                    self.analyze_expr(elem);
                }
            }
            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.analyze_expr(k);
                    self.analyze_expr(v);
                }
            }
            Expr::Keyword(_) => {}

            // Conventional mutable collections
            Expr::ConvVector(elements) => {
                for elem in elements {
                    self.analyze_expr(elem);
                }
            }
            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.analyze_expr(k);
                    self.analyze_expr(v);
                }
            }

            // Async/await
            Expr::Async(body) => {
                self.analyze_expr(body);
            }
            Expr::Await(future) => {
                self.analyze_expr(future);
            }

            // SIMD vectors (ADR-016)
            Expr::SimdVector(elements) => {
                for elem in elements {
                    self.analyze_expr(elem);
                }
            }

            // STM (ADR-012)
            Expr::Dosync(exprs) => {
                for expr in exprs {
                    self.analyze_expr(expr);
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
            } => {
                self.analyze_expr(ref_expr);
                self.analyze_expr(fn_expr);
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
            Expr::Commute {
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

            // Iterators
            Expr::Iter(coll) => {
                self.analyze_expr(coll);
            }
            Expr::Collect(iter) => {
                self.analyze_expr(iter);
            }

            // Byte arrays and regex are literals - no captures
            Expr::ByteArray(_) | Expr::Regex { .. } => {}

            // Overflow handling - recurse into inner expression
            Expr::Boxed(inner) | Expr::Wrapping(inner) => {
                self.analyze_expr(inner);
            }

            // Macro syntax should be expanded before closure analysis
            Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
                self.analyze_expr(inner);
            }
            Expr::Gensym(_) => {}

            // Generated by closure conversion pass (after analysis)
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.analyze_expr(e);
                }
            }
            Expr::RcAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.analyze_expr(value);
                }
            }
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

            // Atom expressions
            Expr::Atom(value) => {
                self.find_free_vars_lambda_inner(value, local, free);
            }
            Expr::Swap(atom, func) => {
                self.find_free_vars_lambda_inner(atom, local, free);
                self.find_free_vars_lambda_inner(func, local, free);
            }
            Expr::Reset(atom, value) => {
                self.find_free_vars_lambda_inner(atom, local, free);
                self.find_free_vars_lambda_inner(value, local, free);
            }
            Expr::AtomDeref(atom) => {
                self.find_free_vars_lambda_inner(atom, local, free);
            }
            Expr::CompareAndSet { atom, old, new } => {
                self.find_free_vars_lambda_inner(atom, local, free);
                self.find_free_vars_lambda_inner(old, local, free);
                self.find_free_vars_lambda_inner(new, local, free);
            }

            // Persistent collections
            Expr::Vector(elements) => {
                for elem in elements {
                    self.find_free_vars_lambda_inner(elem, local, free);
                }
            }
            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.find_free_vars_lambda_inner(k, local, free);
                    self.find_free_vars_lambda_inner(v, local, free);
                }
            }
            Expr::Keyword(_) => {}

            // Conventional mutable collections
            Expr::ConvVector(elements) => {
                for elem in elements {
                    self.find_free_vars_lambda_inner(elem, local, free);
                }
            }
            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.find_free_vars_lambda_inner(k, local, free);
                    self.find_free_vars_lambda_inner(v, local, free);
                }
            }

            // Async/await
            Expr::Async(body) => {
                self.find_free_vars_lambda_inner(body, local, free);
            }
            Expr::Await(future) => {
                self.find_free_vars_lambda_inner(future, local, free);
            }

            // SIMD vectors (ADR-016)
            Expr::SimdVector(elements) => {
                for elem in elements {
                    self.find_free_vars_lambda_inner(elem, local, free);
                }
            }

            // STM (ADR-012)
            Expr::Dosync(exprs) => {
                for expr in exprs {
                    self.find_free_vars_lambda_inner(expr, local, free);
                }
            }
            Expr::RefSetStm(ref_expr, value) => {
                self.find_free_vars_lambda_inner(ref_expr, local, free);
                self.find_free_vars_lambda_inner(value, local, free);
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.find_free_vars_lambda_inner(ref_expr, local, free);
                self.find_free_vars_lambda_inner(fn_expr, local, free);
                for arg in args {
                    self.find_free_vars_lambda_inner(arg, local, free);
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.find_free_vars_lambda_inner(ref_expr, local, free);
                self.find_free_vars_lambda_inner(fn_expr, local, free);
                for arg in args {
                    self.find_free_vars_lambda_inner(arg, local, free);
                }
            }

            // Iterators
            Expr::Iter(coll) => {
                self.find_free_vars_lambda_inner(coll, local, free);
            }
            Expr::Collect(iter) => {
                self.find_free_vars_lambda_inner(iter, local, free);
            }

            // Byte arrays and regex are literals - no free vars
            Expr::ByteArray(_) | Expr::Regex { .. } => {}

            // Overflow handling - recurse into inner expression
            Expr::Boxed(inner) | Expr::Wrapping(inner) => {
                self.find_free_vars_lambda_inner(inner, local, free);
            }

            // Macro syntax should be expanded before this analysis
            Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
                self.find_free_vars_lambda_inner(inner, local, free);
            }
            Expr::Gensym(_) => {}

            // Generated by closure conversion pass (after analysis)
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.find_free_vars_lambda_inner(e, local, free);
                }
            }
            Expr::RcAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.find_free_vars_lambda_inner(value, local, free);
                }
            }
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

/// Analyze closures in a program and return capture information
pub fn analyze(program: &Program) -> Result<HashMap<Span, CaptureInfo>> {
    let analyzer = ClosureAnalyzer::new();
    analyzer.analyze(program)
}

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

            Expr::Match(scrutinee, arms) => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_expr(&arm.body);
                }
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

// =============================================================================
// Closure Conversion Pass
// =============================================================================
//
// Transforms lambdas into lifted functions with explicit environment structs.
// This runs AFTER closure analysis and BEFORE codegen.
//
// The conversion:
// 1. ALL functions (defun) get an env parameter as first argument (ptr, can be null)
// 2. Each lambda becomes:
//    - A lifted top-level function `__lambda_N` with signature `(env: ptr, params...)`
//    - If it has captures: an environment struct `__env_N` with captured values
//    - A ClosureLit { fn_name, env } that creates the closure struct
// 3. Function references (when passed as values) become ClosureLit { fn_name, None }
// 4. Direct function calls add null as the first (env) argument

use std::sync::atomic::{AtomicUsize, Ordering};

/// Counter for generating unique lambda names
static LAMBDA_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn fresh_lambda_name() -> String {
    let n = LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__lambda_{}", n)
}

fn fresh_env_struct_name() -> String {
    let n = LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__env_{}", n)
}

/// Find which variables in a set are used as callables (function position) in an expression
fn find_callable_vars(expr: &Expr, var_names: &HashSet<String>) -> HashSet<String> {
    let mut callable = HashSet::new();
    find_callable_vars_rec(expr, var_names, &mut callable);
    callable
}

fn find_callable_vars_rec(
    expr: &Expr,
    var_names: &HashSet<String>,
    callable: &mut HashSet<String>,
) {
    match expr {
        Expr::Call(func, args) => {
            // If the function is one of our variables, it's callable
            if let Expr::Var(name) = &func.node {
                if var_names.contains(name) {
                    callable.insert(name.clone());
                }
            }
            // Recurse into function expression and arguments
            find_callable_vars_rec(&func.node, var_names, callable);
            for arg in args {
                find_callable_vars_rec(&arg.node, var_names, callable);
            }
        }
        Expr::If(cond, then_, else_) => {
            find_callable_vars_rec(&cond.node, var_names, callable);
            find_callable_vars_rec(&then_.node, var_names, callable);
            find_callable_vars_rec(&else_.node, var_names, callable);
        }
        Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
            for binding in bindings {
                find_callable_vars_rec(&binding.value.node, var_names, callable);
            }
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Do(exprs) => {
            for e in exprs {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::Lambda(_, body) => {
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Match(scrutinee, arms) => {
            find_callable_vars_rec(&scrutinee.node, var_names, callable);
            for arm in arms {
                find_callable_vars_rec(&arm.body.node, var_names, callable);
            }
        }
        Expr::Field(obj, _) => {
            find_callable_vars_rec(&obj.node, var_names, callable);
        }
        Expr::Struct(_, fields) => {
            for (_, value) in fields {
                find_callable_vars_rec(&value.node, var_names, callable);
            }
        }
        Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Set(_, value) => {
            find_callable_vars_rec(&value.node, var_names, callable);
        }
        Expr::Atom(value) => {
            find_callable_vars_rec(&value.node, var_names, callable);
        }
        Expr::Swap(atom, func) | Expr::Reset(atom, func) => {
            find_callable_vars_rec(&atom.node, var_names, callable);
            find_callable_vars_rec(&func.node, var_names, callable);
        }
        Expr::AtomDeref(atom) => {
            find_callable_vars_rec(&atom.node, var_names, callable);
        }
        Expr::CompareAndSet { atom, old, new } => {
            find_callable_vars_rec(&atom.node, var_names, callable);
            find_callable_vars_rec(&old.node, var_names, callable);
            find_callable_vars_rec(&new.node, var_names, callable);
        }
        Expr::Vector(elements) | Expr::ConvVector(elements) | Expr::SimdVector(elements) => {
            for e in elements {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::Map(pairs) | Expr::ConvMap(pairs) => {
            for (k, v) in pairs {
                find_callable_vars_rec(&k.node, var_names, callable);
                find_callable_vars_rec(&v.node, var_names, callable);
            }
        }
        Expr::Async(body) | Expr::Await(body) => {
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Dosync(exprs) => {
            for e in exprs {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::RefSetStm(ref_expr, value) => {
            find_callable_vars_rec(&ref_expr.node, var_names, callable);
            find_callable_vars_rec(&value.node, var_names, callable);
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
            find_callable_vars_rec(&ref_expr.node, var_names, callable);
            find_callable_vars_rec(&fn_expr.node, var_names, callable);
            for arg in args {
                find_callable_vars_rec(&arg.node, var_names, callable);
            }
        }
        Expr::Iter(inner) | Expr::Collect(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Boxed(inner) | Expr::Wrapping(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::ClosureLit { env, .. } => {
            if let Some(e) = env {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::RcAlloc { fields, .. } => {
            for (_, value) in fields {
                find_callable_vars_rec(&value.node, var_names, callable);
            }
        }
        // Leaf nodes - no recursion needed
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::Bool(_)
        | Expr::String(_)
        | Expr::Nil
        | Expr::Var(_)
        | Expr::Quote(_)
        | Expr::Keyword(_)
        | Expr::ByteArray(_)
        | Expr::Regex { .. }
        | Expr::Gensym(_) => {}
    }
}

/// Reset the lambda counter (for testing)
#[cfg(test)]
pub fn reset_lambda_counter() {
    LAMBDA_COUNTER.store(0, Ordering::SeqCst);
}

/// Closure converter state
pub struct ClosureConverter {
    /// Capture info from analysis phase
    capture_info: HashMap<Span, CaptureInfo>,
    /// Generated top-level functions (lifted lambdas)
    generated_functions: Vec<Spanned<Item>>,
    /// Generated struct definitions (environment structs)
    generated_structs: Vec<Spanned<Item>>,
    /// Set of known function names (for detecting function-as-value usage)
    known_functions: HashSet<String>,
}

impl ClosureConverter {
    pub fn new(capture_info: HashMap<Span, CaptureInfo>) -> Self {
        Self {
            capture_info,
            generated_functions: Vec::new(),
            generated_structs: Vec::new(),
            known_functions: HashSet::new(),
        }
    }

    /// Convert a program, transforming all lambdas to lifted functions
    pub fn convert(mut self, program: Program) -> Result<Program> {
        // First pass: collect all known function names
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                self.known_functions.insert(defun.name.node.clone());
            }
        }

        // Second pass: convert all items
        let mut new_items = Vec::new();
        for item in program.items {
            let converted = self.convert_item(item)?;
            new_items.push(converted);
        }

        // Add generated structs first (they need to be defined before functions use them)
        let mut final_items = self.generated_structs;
        final_items.extend(self.generated_functions);
        final_items.extend(new_items);

        Ok(Program { items: final_items })
    }

    fn convert_item(&mut self, item: Spanned<Item>) -> Result<Spanned<Item>> {
        let span = item.span;
        let node = match item.node {
            Item::Defun(defun) => Item::Defun(self.convert_defun(defun)?),
            Item::Def(def) => Item::Def(self.convert_def(def)?),
            Item::ExtendProtocol(extend) => {
                Item::ExtendProtocol(self.convert_extend_protocol(extend)?)
            }
            // These don't need conversion
            Item::Defstruct(s) => Item::Defstruct(s),
            Item::Defprotocol(p) => Item::Defprotocol(p),
            Item::Defmacro(m) => Item::Defmacro(m),
            Item::Extern(e) => Item::Extern(e),
        };
        Ok(Spanned::new(node, span))
    }

    fn convert_defun(&mut self, defun: Defun) -> Result<Defun> {
        // First, find which parameters are used as callables in the body
        // This includes direct calls AND captures used as callables in lambdas
        let param_names: HashSet<String> =
            defun.params.iter().map(|p| p.name.node.clone()).collect();
        let callable_params = find_callable_vars(&defun.body.node, &param_names);

        // Update parameter types for callable params that don't have explicit types
        let new_params: Vec<Param> = defun
            .params
            .into_iter()
            .map(|p| {
                if p.ty.is_none() && callable_params.contains(&p.name.node) {
                    // This parameter is used as a callable - give it Closure type
                    Param {
                        name: p.name.clone(),
                        ty: Some(Spanned::new(crate::ast::Type::Closure, p.name.span)),
                        mutable: p.mutable,
                    }
                } else {
                    p
                }
            })
            .collect();

        // Convert the body, which may contain lambdas
        let new_body = self.convert_expr(defun.body)?;
        Ok(Defun {
            name: defun.name,
            params: new_params,
            return_type: defun.return_type,
            body: new_body,
        })
    }

    fn convert_def(&mut self, def: Def) -> Result<Def> {
        let new_value = self.convert_expr(def.value)?;
        Ok(Def {
            name: def.name,
            value: new_value,
        })
    }

    fn convert_extend_protocol(&mut self, extend: ExtendProtocol) -> Result<ExtendProtocol> {
        let mut new_implementations = Vec::new();
        for method in extend.implementations {
            let new_body = self.convert_expr(method.body)?;
            new_implementations.push(crate::ast::MethodImpl {
                name: method.name,
                params: method.params,
                body: new_body,
            });
        }
        Ok(ExtendProtocol {
            protocol: extend.protocol,
            type_name: extend.type_name,
            implementations: new_implementations,
        })
    }

    fn convert_expr(&mut self, expr: Spanned<Expr>) -> Result<Spanned<Expr>> {
        let span = expr.span;
        let node = match expr.node {
            // Lambda - the main conversion target
            Expr::Lambda(params, body) => {
                return self.convert_lambda(params, *body, span);
            }

            // Recursively convert sub-expressions
            Expr::Call(func, args) => {
                let new_func = Box::new(self.convert_expr(*func)?);
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Call(new_func, new_args?)
            }

            Expr::Let(bindings, body) => {
                let new_bindings = self.convert_bindings(bindings)?;
                let new_body = Box::new(self.convert_expr(*body)?);
                Expr::Let(new_bindings, new_body)
            }

            Expr::Plet(bindings, body) => {
                let new_bindings = self.convert_bindings(bindings)?;
                let new_body = Box::new(self.convert_expr(*body)?);
                Expr::Plet(new_bindings, new_body)
            }

            Expr::If(cond, then_, else_) => {
                let new_cond = Box::new(self.convert_expr(*cond)?);
                let new_then = Box::new(self.convert_expr(*then_)?);
                let new_else = Box::new(self.convert_expr(*else_)?);
                Expr::If(new_cond, new_then, new_else)
            }

            Expr::Do(exprs) => {
                let new_exprs: Result<Vec<_>> =
                    exprs.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Do(new_exprs?)
            }

            Expr::Set(name, value) => {
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::Set(name, new_value)
            }

            Expr::Ref(inner) => Expr::Ref(Box::new(self.convert_expr(*inner)?)),
            Expr::RefMut(inner) => Expr::RefMut(Box::new(self.convert_expr(*inner)?)),
            Expr::Deref(inner) => Expr::Deref(Box::new(self.convert_expr(*inner)?)),
            Expr::Unsafe(inner) => Expr::Unsafe(Box::new(self.convert_expr(*inner)?)),

            Expr::Struct(name, fields) => {
                let new_fields: Result<Vec<_>> = fields
                    .into_iter()
                    .map(|(n, v)| Ok((n, self.convert_expr(v)?)))
                    .collect();
                Expr::Struct(name, new_fields?)
            }

            Expr::Field(obj, field) => {
                let new_obj = Box::new(self.convert_expr(*obj)?);
                Expr::Field(new_obj, field)
            }

            Expr::Match(scrutinee, arms) => {
                let new_scrutinee = Box::new(self.convert_expr(*scrutinee)?);
                let new_arms: Result<Vec<_>> = arms
                    .into_iter()
                    .map(|arm| {
                        let new_body = self.convert_expr(arm.body)?;
                        Ok(MatchArm {
                            pattern: arm.pattern,
                            body: new_body,
                        })
                    })
                    .collect();
                Expr::Match(new_scrutinee, new_arms?)
            }

            // Atom expressions
            Expr::Atom(value) => Expr::Atom(Box::new(self.convert_expr(*value)?)),
            Expr::Swap(atom, func) => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_func = Box::new(self.convert_expr(*func)?);
                Expr::Swap(new_atom, new_func)
            }
            Expr::Reset(atom, value) => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::Reset(new_atom, new_value)
            }
            Expr::AtomDeref(atom) => Expr::AtomDeref(Box::new(self.convert_expr(*atom)?)),
            Expr::CompareAndSet { atom, old, new } => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_old = Box::new(self.convert_expr(*old)?);
                let new_new = Box::new(self.convert_expr(*new)?);
                Expr::CompareAndSet {
                    atom: new_atom,
                    old: new_old,
                    new: new_new,
                }
            }

            // Collections
            Expr::Vector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Vector(new_elements?)
            }
            Expr::Map(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .into_iter()
                    .map(|(k, v)| Ok((self.convert_expr(k)?, self.convert_expr(v)?)))
                    .collect();
                Expr::Map(new_pairs?)
            }
            Expr::ConvVector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::ConvVector(new_elements?)
            }
            Expr::ConvMap(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .into_iter()
                    .map(|(k, v)| Ok((self.convert_expr(k)?, self.convert_expr(v)?)))
                    .collect();
                Expr::ConvMap(new_pairs?)
            }

            // Async
            Expr::Async(body) => Expr::Async(Box::new(self.convert_expr(*body)?)),
            Expr::Await(future) => Expr::Await(Box::new(self.convert_expr(*future)?)),

            // SIMD
            Expr::SimdVector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::SimdVector(new_elements?)
            }

            // STM
            Expr::Dosync(exprs) => {
                let new_exprs: Result<Vec<_>> =
                    exprs.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Dosync(new_exprs?)
            }
            Expr::RefSetStm(ref_expr, value) => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::RefSetStm(new_ref, new_value)
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_fn = Box::new(self.convert_expr(*fn_expr)?);
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Alter {
                    ref_expr: new_ref,
                    fn_expr: new_fn,
                    args: new_args?,
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_fn = Box::new(self.convert_expr(*fn_expr)?);
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Commute {
                    ref_expr: new_ref,
                    fn_expr: new_fn,
                    args: new_args?,
                }
            }

            // Iterators
            Expr::Iter(coll) => Expr::Iter(Box::new(self.convert_expr(*coll)?)),
            Expr::Collect(iter) => Expr::Collect(Box::new(self.convert_expr(*iter)?)),

            // Overflow handling
            Expr::Boxed(inner) => Expr::Boxed(Box::new(self.convert_expr(*inner)?)),
            Expr::Wrapping(inner) => Expr::Wrapping(Box::new(self.convert_expr(*inner)?)),

            // Macro syntax (should be expanded before this pass)
            Expr::Quasiquote(inner) => Expr::Quasiquote(Box::new(self.convert_expr(*inner)?)),
            Expr::Unquote(inner) => Expr::Unquote(Box::new(self.convert_expr(*inner)?)),
            Expr::UnquoteSplicing(inner) => {
                Expr::UnquoteSplicing(Box::new(self.convert_expr(*inner)?))
            }

            // Already converted (shouldn't happen, but handle gracefully)
            Expr::ClosureLit { fn_name, env } => {
                let new_env = match env {
                    Some(e) => Some(Box::new(self.convert_expr(*e)?)),
                    None => None,
                };
                Expr::ClosureLit {
                    fn_name,
                    env: new_env,
                }
            }
            Expr::RcAlloc {
                struct_name,
                fields,
            } => {
                let new_fields: Result<Vec<_>> = fields
                    .into_iter()
                    .map(|(n, v)| Ok((n, self.convert_expr(v)?)))
                    .collect();
                Expr::RcAlloc {
                    struct_name,
                    fields: new_fields?,
                }
            }

            // Literals - no conversion needed
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Keyword(_)
            | Expr::Quote(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. }
            | Expr::Gensym(_) => expr.node,
        };
        Ok(Spanned::new(node, span))
    }

    fn convert_bindings(&mut self, bindings: Vec<LetBinding>) -> Result<Vec<LetBinding>> {
        bindings
            .into_iter()
            .map(|binding| {
                let new_value = self.convert_expr(binding.value)?;
                Ok(LetBinding {
                    name: binding.name,
                    ty: binding.ty,
                    value: new_value,
                })
            })
            .collect()
    }

    /// Convert a lambda to a ClosureLit
    fn convert_lambda(
        &mut self,
        params: Vec<Param>,
        body: Spanned<Expr>,
        span: Span,
    ) -> Result<Spanned<Expr>> {
        // Get capture info for this lambda
        let capture_info = self.capture_info.get(&span).cloned().unwrap_or_default();

        // Generate unique names
        let fn_name = fresh_lambda_name();

        // Convert the body (may contain nested lambdas)
        let converted_body = self.convert_expr(body)?;

        // Track the env struct name (if any) for use in RcAlloc later
        let mut env_struct_name_for_alloc: Option<String> = None;

        // Build the lifted function
        let lifted_fn = if capture_info.captures.is_empty() {
            // No captures - simple case
            // Function signature: (params...) -> result
            Defun {
                name: Spanned::new(fn_name.clone(), span),
                params: params.clone(),
                return_type: None,
                body: converted_body,
            }
        } else {
            // Has captures - need environment struct
            let env_struct_name = fresh_env_struct_name();
            env_struct_name_for_alloc = Some(env_struct_name.clone());

            // Find which captures are used as callables in the body
            let capture_names: HashSet<String> = capture_info
                .captures
                .iter()
                .map(|c| c.name.clone())
                .collect();
            let callable_captures = find_callable_vars(&converted_body.node, &capture_names);

            // Create the environment struct definition
            // Use Type::Closure for callable captures, i64 for others
            let env_fields: Vec<crate::ast::StructField> = capture_info
                .captures
                .iter()
                .map(|cap| {
                    let ty = if callable_captures.contains(&cap.name) {
                        // This capture is called as a function - it's a closure struct
                        crate::ast::Type::Closure
                    } else {
                        // Default to i64 for non-callable captures
                        crate::ast::Type::Named("i64".to_string())
                    };
                    crate::ast::StructField {
                        name: Spanned::new(cap.name.clone(), cap.span),
                        ty: Spanned::new(ty, cap.span),
                    }
                })
                .collect();

            let env_struct = crate::ast::Defstruct {
                name: Spanned::new(env_struct_name.clone(), span),
                fields: env_fields,
            };
            self.generated_structs
                .push(Spanned::new(Item::Defstruct(env_struct), span));

            // Create body that accesses captures from env parameter
            // We need to wrap the body to extract captured variables from the env
            let body_with_env_access = self.wrap_body_with_env_access(
                converted_body,
                &env_struct_name,
                &capture_info.captures,
                span,
            );

            // Add env parameter at the start with the actual struct type (not just ptr)
            // This allows codegen to know the struct type for field access
            let mut lifted_params = vec![Param {
                name: Spanned::new("__env".to_string(), span),
                ty: Some(Spanned::new(
                    crate::ast::Type::Named(env_struct_name.clone()),
                    span,
                )),
                mutable: false,
            }];
            lifted_params.extend(params.clone());

            Defun {
                name: Spanned::new(fn_name.clone(), span),
                params: lifted_params,
                return_type: None,
                body: body_with_env_access,
            }
        };

        // Add the lifted function to generated functions
        self.generated_functions
            .push(Spanned::new(Item::Defun(lifted_fn), span));

        // Create the ClosureLit expression
        let env_expr = if let Some(env_struct_name) = env_struct_name_for_alloc {
            // Create RcAlloc for the environment
            let env_fields: Vec<(Spanned<String>, Spanned<Expr>)> = capture_info
                .captures
                .iter()
                .map(|cap| {
                    (
                        Spanned::new(cap.name.clone(), cap.span),
                        Spanned::new(Expr::Var(cap.name.clone()), cap.span),
                    )
                })
                .collect();

            Some(Box::new(Spanned::new(
                Expr::RcAlloc {
                    struct_name: env_struct_name,
                    fields: env_fields,
                },
                span,
            )))
        } else {
            None
        };

        Ok(Spanned::new(
            Expr::ClosureLit {
                fn_name,
                env: env_expr,
            },
            span,
        ))
    }

    /// Wrap a lambda body to extract captured variables from the env parameter
    fn wrap_body_with_env_access(
        &self,
        body: Spanned<Expr>,
        _env_struct_name: &str,
        captures: &[Capture],
        span: Span,
    ) -> Spanned<Expr> {
        if captures.is_empty() {
            return body;
        }

        // Create let bindings to extract each capture from the env
        // (let ((x (. __env x)) (y (. __env y))) body)
        let bindings: Vec<LetBinding> = captures
            .iter()
            .map(|cap| {
                let field_access = Expr::Field(
                    Box::new(Spanned::new(Expr::Var("__env".to_string()), span)),
                    Spanned::new(cap.name.clone(), cap.span),
                );
                LetBinding {
                    name: Spanned::new(cap.name.clone(), cap.span),
                    ty: None,
                    value: Spanned::new(field_access, cap.span),
                }
            })
            .collect();

        Spanned::new(Expr::Let(bindings, Box::new(body)), span)
    }
}

/// Convert all lambdas in a program to lifted functions with explicit environments
pub fn convert(program: Program, capture_info: HashMap<Span, CaptureInfo>) -> Result<Program> {
    let converter = ClosureConverter::new(capture_info);
    converter.convert(program)
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

    #[test]
    fn test_plet_non_atom_mutation_error() {
        // set! on non-atom binding in plet is an error
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter 0))
                (set! counter (+ counter 1))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("cannot mutate non-atom binding"));
        assert!(errors[0].message.contains("counter"));
    }

    #[test]
    fn test_plet_atom_mutation_ok() {
        // set! via atom operations (swap!, reset!) is OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (reset! counter 1)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_with_atom_swap_ok() {
        // Using swap! with atoms in plet is OK
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (swap! counter inc)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_plet_respects_scopes() {
        // Atom bindings from outer plet should be visible in inner code
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (do
                  (reset! counter 1)
                  @counter)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_pure_closure_ok() {
        // Pure closures (no captures) are OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (pmap (fn (x) (* x x)) data))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_sync_closure_ok() {
        // Sync closures (move captures) are OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((factor 2))
                (pmap (fn (x) (* x factor)) data)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_local_closure_error() {
        // Local closures (borrow captures) are NOT OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((factor 2))
                (pmap (fn (x) (* (deref (ref factor)) x)) data)))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("pmap"));
        assert!(errors[0].message.contains("thread-safe"));
    }

    #[test]
    fn test_pfilter_requires_sync() {
        // pfilter also requires Sync closures
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((x 1))
                (pfilter (fn (y) (= (deref (ref x)) y)) data)))
            "#,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].message.contains("pfilter"));
    }

    // ==========================================================================
    // Closure Conversion Tests
    // ==========================================================================

    fn convert_source(source: &str) -> Program {
        reset_lambda_counter();
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        let capture_info = ClosureAnalyzer::new().analyze(&program).unwrap();
        ClosureConverter::new(capture_info)
            .convert(program)
            .expect("conversion failed")
    }

    fn find_defun<'a>(program: &'a Program, name: &str) -> Option<&'a crate::ast::Defun> {
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                if defun.name.node == name {
                    return Some(defun);
                }
            }
        }
        None
    }

    fn count_items_of_type(program: &Program, matcher: fn(&Item) -> bool) -> usize {
        program.items.iter().filter(|i| matcher(&i.node)).count()
    }

    #[test]
    fn test_convert_pure_lambda() {
        // A pure lambda (no captures) should become a lifted function with ClosureLit
        let program = convert_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) x)))
                f))
            "#,
        );

        // Should have original defun + generated lambda function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 2, "Expected 2 functions (foo + lifted lambda)");

        // A lifted lambda should exist (name starts with __lambda_)
        let has_lifted = program.items.iter().any(|item| {
            if let Item::Defun(defun) = &item.node {
                defun.name.node.starts_with("__lambda_")
            } else {
                false
            }
        });
        assert!(has_lifted, "Should have generated a lifted lambda function");
    }

    #[test]
    fn test_convert_lambda_with_capture() {
        // A lambda with captures should generate an env struct and ClosureLit with RcAlloc
        let program = convert_source(
            r#"
            (defun constantly (v)
              (fn (x) v))
            "#,
        );

        // Should have: original defun + generated lambda function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(
            defun_count, 2,
            "Expected 2 functions (constantly + lifted lambda)"
        );

        // Should have generated env struct
        let struct_count = count_items_of_type(&program, |i| matches!(i, Item::Defstruct(_)));
        assert!(struct_count >= 1, "Expected at least 1 struct (__env_N)");

        // Find the lifted lambda (starts with __lambda_)
        let lambda = program
            .items
            .iter()
            .find_map(|item| {
                if let Item::Defun(defun) = &item.node {
                    if defun.name.node.starts_with("__lambda_") {
                        return Some(defun);
                    }
                }
                None
            })
            .expect("Should have a lifted lambda");

        assert!(
            !lambda.params.is_empty(),
            "Lambda should have at least one parameter"
        );
        assert_eq!(
            lambda.params[0].name.node, "__env",
            "First param should be __env"
        );
    }

    #[test]
    fn test_convert_preserves_function_body() {
        // The original function's body should still work (modulo the lambda transformation)
        let program = convert_source(
            r#"
            (defun add (a b) (+ a b))
            "#,
        );

        // Should have exactly one function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 1, "Expected 1 function");

        // The function should be preserved
        let add = find_defun(&program, "add").expect("Should have add function");
        assert_eq!(add.params.len(), 2);
    }

    #[test]
    fn test_convert_nested_lambdas() {
        // Nested lambdas should each become their own lifted function
        let program = convert_source(
            r#"
            (defun comp (f g)
              (fn (x) (f (g x))))
            "#,
        );

        // Should have: comp + lifted lambda
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 2, "Expected 2 functions");

        // Find the lifted lambda
        let lambda = program
            .items
            .iter()
            .find_map(|item| {
                if let Item::Defun(defun) = &item.node {
                    if defun.name.node.starts_with("__lambda_") {
                        return Some(defun);
                    }
                }
                None
            })
            .expect("Should have a lifted lambda");

        // Should have __env param (captures f, g) plus x
        assert!(!lambda.params.is_empty(), "Lambda should have params");
    }
}
