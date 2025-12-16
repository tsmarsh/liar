//! Closure analysis
//!
//! Determines what variables closures capture and how (by value, by ref, by mut ref).

use std::collections::{HashMap, HashSet};

use crate::ast::{Def, Defun, Expr, ExtendProtocol, Item, LetBinding, Param, Program};
use crate::error::Result;
use crate::resolve::BindingId;
use crate::span::{Span, Spanned};

use super::types::{Capture, CaptureInfo, CaptureMode, ClosureColor};

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
            Item::ExtendProtocolDefault(extend) => {
                // Analyze method bodies just like ExtendProtocol
                for method in &extend.implementations {
                    self.push_scope();
                    for param in &method.params {
                        self.define(&param.node);
                    }
                    self.analyze_expr(&method.body);
                    self.pop_scope();
                }
            }
            // Macros should be expanded before closure analysis
            Item::Defmacro(_) => {}
            // Extern declarations have no body
            Item::Extern(_) => {}
            // Namespace declarations have no body
            Item::Namespace(_) => {}
            // WhenTarget should be flattened before closure analysis
            Item::WhenTarget(_) => {}
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

            // Type predicates
            Expr::Instance(obj, _) => {
                self.analyze_expr(obj);
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
            Expr::HeapEnvAlloc { fields, .. } | Expr::StackEnvAlloc { fields, .. } => {
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
                // Only capture unqualified names (qualified names are module-level)
                if name.is_simple()
                    && !local.contains(&name.name)
                    && !self.is_builtin(&name.name)
                    && !free.iter().any(|(n, _)| n == &name.name)
                {
                    free.push((name.name.clone(), expr.span));
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

            // Type predicates
            Expr::Instance(obj, _) => {
                self.find_free_vars_lambda_inner(obj, local, free);
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
            Expr::HeapEnvAlloc { fields, .. } | Expr::StackEnvAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.find_free_vars_lambda_inner(value, local, free);
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
                    if var_name.name == name {
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
