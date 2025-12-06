//! Macro expansion
//!
//! Expands macro invocations before type inference.
//! This pass:
//! 1. Collects macro definitions
//! 2. Expands macro calls in all expressions
//! 3. Removes macro definitions from the program (they don't generate code)

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::{Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::span::{Span, Spanned};

/// Global counter for gensym
static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique symbol
fn gensym(prefix: Option<&str>) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    match prefix {
        Some(p) => format!("{}_{}", p, n),
        None => format!("G__{}", n),
    }
}

/// A macro definition
#[derive(Clone)]
struct MacroDef {
    params: Vec<String>,
    body: Spanned<Expr>,
}

/// Macro expander
#[derive(Default)]
pub struct Expander {
    macros: HashMap<String, MacroDef>,
}

impl Expander {
    pub fn new() -> Self {
        Self::default()
    }

    /// Collect macro definitions from the program
    fn collect_macros(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Defmacro(defmacro) = &item.node {
                let def = MacroDef {
                    params: defmacro.params.iter().map(|p| p.node.clone()).collect(),
                    body: defmacro.body.clone(),
                };
                self.macros.insert(defmacro.name.node.clone(), def);
            }
        }
    }

    /// Expand all macros in a program
    pub fn expand_program(&mut self, program: &mut Program) -> Result<()> {
        // First, collect all macro definitions
        self.collect_macros(program);

        // Expand macros in all items
        for item in &mut program.items {
            self.expand_item(item)?;
        }

        // Remove macro definitions from the program (they don't generate code)
        program
            .items
            .retain(|item| !matches!(item.node, Item::Defmacro(_)));

        Ok(())
    }

    fn expand_item(&mut self, item: &mut Spanned<Item>) -> Result<()> {
        match &mut item.node {
            Item::Defun(defun) => {
                self.expand_expr(&mut defun.body)?;
            }
            Item::Def(def) => {
                self.expand_expr(&mut def.value)?;
            }
            Item::Defmacro(_) => {
                // Macro definitions are collected, not expanded
            }
            Item::Defstruct(_) => {}
            Item::Defprotocol(_) => {}
            Item::ExtendProtocol(extend) => {
                for method in &mut extend.implementations {
                    self.expand_expr(&mut method.body)?;
                }
            }
            Item::Extern(_) => {
                // Extern declarations have no body to expand
            }
        }
        Ok(())
    }

    fn expand_expr(&mut self, expr: &mut Spanned<Expr>) -> Result<()> {
        // First, check if this is a macro call
        if let Expr::Call(func, args) = &expr.node {
            if let Expr::Var(name) = &func.node {
                if let Some(macro_def) = self.macros.get(name).cloned() {
                    // This is a macro call - expand it
                    let expanded = self.expand_macro_call(&macro_def, args, expr.span)?;
                    *expr = expanded;
                    // Recursively expand the result
                    return self.expand_expr(expr);
                }
            }
        }

        // Not a macro call, recursively expand sub-expressions
        match &mut expr.node {
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Keyword(_)
            | Expr::Quote(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. } => {}

            Expr::Call(func, args) => {
                self.expand_expr(func)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }

            Expr::Lambda(_, body) => {
                self.expand_expr(body)?;
            }

            Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
                for binding in bindings {
                    self.expand_expr(&mut binding.value)?;
                }
                self.expand_expr(body)?;
            }

            Expr::If(cond, then_, else_) => {
                self.expand_expr(cond)?;
                self.expand_expr(then_)?;
                self.expand_expr(else_)?;
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.expand_expr(e)?;
                }
            }

            Expr::Set(_, value) => {
                self.expand_expr(value)?;
            }

            Expr::Ref(inner)
            | Expr::RefMut(inner)
            | Expr::Deref(inner)
            | Expr::Unsafe(inner)
            | Expr::Boxed(inner)
            | Expr::Wrapping(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    self.expand_expr(value)?;
                }
            }

            Expr::Field(obj, _) => {
                self.expand_expr(obj)?;
            }

            Expr::Match(scrutinee, arms) => {
                self.expand_expr(scrutinee)?;
                for arm in arms {
                    self.expand_expr(&mut arm.body)?;
                }
            }

            Expr::Atom(value) => {
                self.expand_expr(value)?;
            }
            Expr::AtomDeref(atom) => {
                self.expand_expr(atom)?;
            }
            Expr::Reset(atom, value) => {
                self.expand_expr(atom)?;
                self.expand_expr(value)?;
            }
            Expr::Swap(atom, func) => {
                self.expand_expr(atom)?;
                self.expand_expr(func)?;
            }
            Expr::CompareAndSet { atom, old, new } => {
                self.expand_expr(atom)?;
                self.expand_expr(old)?;
                self.expand_expr(new)?;
            }

            Expr::Vector(elements) | Expr::ConvVector(elements) | Expr::SimdVector(elements) => {
                for elem in elements {
                    self.expand_expr(elem)?;
                }
            }
            Expr::Map(pairs) | Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.expand_expr(k)?;
                    self.expand_expr(v)?;
                }
            }

            Expr::Async(body) => {
                self.expand_expr(body)?;
            }
            Expr::Await(future) => {
                self.expand_expr(future)?;
            }

            Expr::Dosync(exprs) => {
                for e in exprs {
                    self.expand_expr(e)?;
                }
            }
            Expr::RefSetStm(ref_expr, value) => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(value)?;
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(fn_expr)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(fn_expr)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }

            Expr::Iter(coll) => {
                self.expand_expr(coll)?;
            }
            Expr::Collect(iter) => {
                self.expand_expr(iter)?;
            }

            // These should not appear in user code, they're macro syntax
            Expr::Quasiquote(_) | Expr::Unquote(_) | Expr::UnquoteSplicing(_) | Expr::Gensym(_) => {
                return Err(CompileError::macro_error(
                    expr.span,
                    "quasiquote syntax outside macro definition",
                ));
            }

            // Generated by closure conversion pass (after macro expansion)
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.expand_expr(e)?;
                }
            }
            Expr::RcAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.expand_expr(value)?;
                }
            }
        }

        Ok(())
    }

    /// Expand a macro call
    fn expand_macro_call(
        &self,
        macro_def: &MacroDef,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Spanned<Expr>> {
        if args.len() != macro_def.params.len() {
            return Err(CompileError::macro_error(
                span,
                format!(
                    "macro expects {} arguments, got {}",
                    macro_def.params.len(),
                    args.len()
                ),
            ));
        }

        // Build substitution environment
        let mut env: HashMap<String, Spanned<Expr>> = HashMap::new();
        for (param, arg) in macro_def.params.iter().zip(args.iter()) {
            env.insert(param.clone(), arg.clone());
        }

        // Substitute in the macro body
        self.substitute(&macro_def.body, &env, span)
    }

    /// Substitute macro parameters in an expression
    fn substitute(
        &self,
        expr: &Spanned<Expr>,
        env: &HashMap<String, Spanned<Expr>>,
        call_span: Span,
    ) -> Result<Spanned<Expr>> {
        match &expr.node {
            // Variable - might be a macro parameter
            Expr::Var(name) => {
                if let Some(replacement) = env.get(name) {
                    Ok(replacement.clone())
                } else {
                    Ok(expr.clone())
                }
            }

            // Quasiquote - this is where the magic happens
            Expr::Quasiquote(inner) => self.expand_quasiquote(inner, env, call_span),

            // Unquote/UnquoteSplicing outside quasiquote is an error
            Expr::Unquote(_) | Expr::UnquoteSplicing(_) => Err(CompileError::macro_error(
                expr.span,
                "unquote outside quasiquote",
            )),

            // Gensym - generate a unique symbol
            Expr::Gensym(prefix) => {
                let sym = gensym(prefix.as_deref());
                Ok(Spanned::new(Expr::Var(sym), expr.span))
            }

            // Literals - no substitution needed
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Keyword(_)
            | Expr::Quote(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. } => Ok(expr.clone()),

            // Recursive cases - substitute in sub-expressions
            Expr::Call(func, args) => {
                let new_func = self.substitute(func, env, call_span)?;
                let new_args: Result<Vec<_>> = args
                    .iter()
                    .map(|a| self.substitute(a, env, call_span))
                    .collect();
                Ok(Spanned::new(
                    Expr::Call(Box::new(new_func), new_args?),
                    expr.span,
                ))
            }

            Expr::Lambda(params, body) => {
                // Shadow macro parameters with lambda parameters
                let mut new_env = env.clone();
                for param in params {
                    new_env.remove(&param.name.node);
                }
                let new_body = self.substitute(body, &new_env, call_span)?;
                Ok(Spanned::new(
                    Expr::Lambda(params.clone(), Box::new(new_body)),
                    expr.span,
                ))
            }

            Expr::Let(bindings, body) => {
                let mut new_env = env.clone();
                let mut new_bindings = Vec::new();
                for binding in bindings {
                    let new_value = self.substitute(&binding.value, &new_env, call_span)?;
                    new_bindings.push(crate::ast::LetBinding {
                        name: binding.name.clone(),
                        ty: binding.ty.clone(),
                        value: new_value,
                    });
                    // Shadow the binding
                    new_env.remove(&binding.name.node);
                }
                let new_body = self.substitute(body, &new_env, call_span)?;
                Ok(Spanned::new(
                    Expr::Let(new_bindings, Box::new(new_body)),
                    expr.span,
                ))
            }

            Expr::Plet(bindings, body) => {
                let mut new_env = env.clone();
                let mut new_bindings = Vec::new();
                for binding in bindings {
                    let new_value = self.substitute(&binding.value, &new_env, call_span)?;
                    new_bindings.push(crate::ast::LetBinding {
                        name: binding.name.clone(),
                        ty: binding.ty.clone(),
                        value: new_value,
                    });
                    new_env.remove(&binding.name.node);
                }
                let new_body = self.substitute(body, &new_env, call_span)?;
                Ok(Spanned::new(
                    Expr::Plet(new_bindings, Box::new(new_body)),
                    expr.span,
                ))
            }

            Expr::If(cond, then_, else_) => {
                let new_cond = self.substitute(cond, env, call_span)?;
                let new_then = self.substitute(then_, env, call_span)?;
                let new_else = self.substitute(else_, env, call_span)?;
                Ok(Spanned::new(
                    Expr::If(Box::new(new_cond), Box::new(new_then), Box::new(new_else)),
                    expr.span,
                ))
            }

            Expr::Do(exprs) => {
                let new_exprs: Result<Vec<_>> = exprs
                    .iter()
                    .map(|e| self.substitute(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::Do(new_exprs?), expr.span))
            }

            Expr::Set(name, value) => {
                let new_value = self.substitute(value, env, call_span)?;
                Ok(Spanned::new(
                    Expr::Set(name.clone(), Box::new(new_value)),
                    expr.span,
                ))
            }

            Expr::Ref(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::Ref(Box::new(new_inner)), expr.span))
            }
            Expr::RefMut(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::RefMut(Box::new(new_inner)), expr.span))
            }
            Expr::Deref(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::Deref(Box::new(new_inner)), expr.span))
            }
            Expr::Unsafe(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::Unsafe(Box::new(new_inner)), expr.span))
            }
            Expr::Boxed(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::Boxed(Box::new(new_inner)), expr.span))
            }
            Expr::Wrapping(inner) => {
                let new_inner = self.substitute(inner, env, call_span)?;
                Ok(Spanned::new(Expr::Wrapping(Box::new(new_inner)), expr.span))
            }

            Expr::Struct(name, fields) => {
                let new_fields: Result<Vec<_>> = fields
                    .iter()
                    .map(|(n, v)| {
                        let new_v = self.substitute(v, env, call_span)?;
                        Ok((n.clone(), new_v))
                    })
                    .collect();
                Ok(Spanned::new(
                    Expr::Struct(name.clone(), new_fields?),
                    expr.span,
                ))
            }

            Expr::Field(obj, field) => {
                let new_obj = self.substitute(obj, env, call_span)?;
                Ok(Spanned::new(
                    Expr::Field(Box::new(new_obj), field.clone()),
                    expr.span,
                ))
            }

            Expr::Match(scrutinee, arms) => {
                let new_scrutinee = self.substitute(scrutinee, env, call_span)?;
                let new_arms: Result<Vec<_>> = arms
                    .iter()
                    .map(|arm| {
                        // Pattern bindings shadow macro parameters
                        let mut arm_env = env.clone();
                        self.remove_pattern_bindings(&arm.pattern.node, &mut arm_env);
                        let new_body = self.substitute(&arm.body, &arm_env, call_span)?;
                        Ok(crate::ast::MatchArm {
                            pattern: arm.pattern.clone(),
                            body: new_body,
                        })
                    })
                    .collect();
                Ok(Spanned::new(
                    Expr::Match(Box::new(new_scrutinee), new_arms?),
                    expr.span,
                ))
            }

            Expr::Atom(value) => {
                let new_value = self.substitute(value, env, call_span)?;
                Ok(Spanned::new(Expr::Atom(Box::new(new_value)), expr.span))
            }
            Expr::AtomDeref(atom) => {
                let new_atom = self.substitute(atom, env, call_span)?;
                Ok(Spanned::new(Expr::AtomDeref(Box::new(new_atom)), expr.span))
            }
            Expr::Reset(atom, value) => {
                let new_atom = self.substitute(atom, env, call_span)?;
                let new_value = self.substitute(value, env, call_span)?;
                Ok(Spanned::new(
                    Expr::Reset(Box::new(new_atom), Box::new(new_value)),
                    expr.span,
                ))
            }
            Expr::Swap(atom, func) => {
                let new_atom = self.substitute(atom, env, call_span)?;
                let new_func = self.substitute(func, env, call_span)?;
                Ok(Spanned::new(
                    Expr::Swap(Box::new(new_atom), Box::new(new_func)),
                    expr.span,
                ))
            }
            Expr::CompareAndSet { atom, old, new } => {
                let new_atom = self.substitute(atom, env, call_span)?;
                let new_old = self.substitute(old, env, call_span)?;
                let new_new = self.substitute(new, env, call_span)?;
                Ok(Spanned::new(
                    Expr::CompareAndSet {
                        atom: Box::new(new_atom),
                        old: Box::new(new_old),
                        new: Box::new(new_new),
                    },
                    expr.span,
                ))
            }

            Expr::Vector(elements) => {
                let new_elements: Result<Vec<_>> = elements
                    .iter()
                    .map(|e| self.substitute(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::Vector(new_elements?), expr.span))
            }
            Expr::ConvVector(elements) => {
                let new_elements: Result<Vec<_>> = elements
                    .iter()
                    .map(|e| self.substitute(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::ConvVector(new_elements?), expr.span))
            }
            Expr::SimdVector(elements) => {
                let new_elements: Result<Vec<_>> = elements
                    .iter()
                    .map(|e| self.substitute(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::SimdVector(new_elements?), expr.span))
            }
            Expr::Map(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .iter()
                    .map(|(k, v)| {
                        let new_k = self.substitute(k, env, call_span)?;
                        let new_v = self.substitute(v, env, call_span)?;
                        Ok((new_k, new_v))
                    })
                    .collect();
                Ok(Spanned::new(Expr::Map(new_pairs?), expr.span))
            }
            Expr::ConvMap(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .iter()
                    .map(|(k, v)| {
                        let new_k = self.substitute(k, env, call_span)?;
                        let new_v = self.substitute(v, env, call_span)?;
                        Ok((new_k, new_v))
                    })
                    .collect();
                Ok(Spanned::new(Expr::ConvMap(new_pairs?), expr.span))
            }

            Expr::Async(body) => {
                let new_body = self.substitute(body, env, call_span)?;
                Ok(Spanned::new(Expr::Async(Box::new(new_body)), expr.span))
            }
            Expr::Await(future) => {
                let new_future = self.substitute(future, env, call_span)?;
                Ok(Spanned::new(Expr::Await(Box::new(new_future)), expr.span))
            }

            Expr::Dosync(exprs) => {
                let new_exprs: Result<Vec<_>> = exprs
                    .iter()
                    .map(|e| self.substitute(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::Dosync(new_exprs?), expr.span))
            }
            Expr::RefSetStm(ref_expr, value) => {
                let new_ref = self.substitute(ref_expr, env, call_span)?;
                let new_value = self.substitute(value, env, call_span)?;
                Ok(Spanned::new(
                    Expr::RefSetStm(Box::new(new_ref), Box::new(new_value)),
                    expr.span,
                ))
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = self.substitute(ref_expr, env, call_span)?;
                let new_fn = self.substitute(fn_expr, env, call_span)?;
                let new_args: Result<Vec<_>> = args
                    .iter()
                    .map(|a| self.substitute(a, env, call_span))
                    .collect();
                Ok(Spanned::new(
                    Expr::Alter {
                        ref_expr: Box::new(new_ref),
                        fn_expr: Box::new(new_fn),
                        args: new_args?,
                    },
                    expr.span,
                ))
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = self.substitute(ref_expr, env, call_span)?;
                let new_fn = self.substitute(fn_expr, env, call_span)?;
                let new_args: Result<Vec<_>> = args
                    .iter()
                    .map(|a| self.substitute(a, env, call_span))
                    .collect();
                Ok(Spanned::new(
                    Expr::Commute {
                        ref_expr: Box::new(new_ref),
                        fn_expr: Box::new(new_fn),
                        args: new_args?,
                    },
                    expr.span,
                ))
            }

            Expr::Iter(coll) => {
                let new_coll = self.substitute(coll, env, call_span)?;
                Ok(Spanned::new(Expr::Iter(Box::new(new_coll)), expr.span))
            }
            Expr::Collect(iter) => {
                let new_iter = self.substitute(iter, env, call_span)?;
                Ok(Spanned::new(Expr::Collect(Box::new(new_iter)), expr.span))
            }

            // Generated by closure conversion pass (after macro expansion)
            Expr::ClosureLit {
                fn_name,
                env: closure_env,
            } => {
                let new_env = match closure_env {
                    Some(e) => Some(Box::new(self.substitute(e, env, call_span)?)),
                    None => None,
                };
                Ok(Spanned::new(
                    Expr::ClosureLit {
                        fn_name: fn_name.clone(),
                        env: new_env,
                    },
                    expr.span,
                ))
            }
            Expr::RcAlloc {
                struct_name,
                fields,
            } => {
                let new_fields: Result<Vec<_>> = fields
                    .iter()
                    .map(|(name, value)| {
                        Ok((name.clone(), self.substitute(value, env, call_span)?))
                    })
                    .collect();
                Ok(Spanned::new(
                    Expr::RcAlloc {
                        struct_name: struct_name.clone(),
                        fields: new_fields?,
                    },
                    expr.span,
                ))
            }
        }
    }

    /// Expand quasiquote - handles `, ,, and ,@
    fn expand_quasiquote(
        &self,
        expr: &Spanned<Expr>,
        env: &HashMap<String, Spanned<Expr>>,
        call_span: Span,
    ) -> Result<Spanned<Expr>> {
        match &expr.node {
            // Unquote - substitute and return the expression
            Expr::Unquote(inner) => self.substitute(inner, env, call_span),

            // UnquoteSplicing - only valid in list context, error here
            Expr::UnquoteSplicing(_) => Err(CompileError::macro_error(
                expr.span,
                "unquote-splicing (,@) must be in list context",
            )),

            // Nested quasiquote - keep it as quasiquote (for now, don't support nested)
            Expr::Quasiquote(_) => Err(CompileError::macro_error(
                expr.span,
                "nested quasiquote not yet supported",
            )),

            // Call - need to handle ,@ in argument list
            Expr::Call(func, args) => {
                let new_func = self.expand_quasiquote(func, env, call_span)?;
                let mut new_args = Vec::new();
                for arg in args {
                    if let Expr::UnquoteSplicing(inner) = &arg.node {
                        // Splice the inner expression (must be a list/vector)
                        let spliced = self.substitute(inner, env, call_span)?;
                        // For now, we expect the spliced value to be expanded at runtime
                        // This is a simplification - full implementation would need runtime support
                        new_args.push(spliced);
                    } else {
                        new_args.push(self.expand_quasiquote(arg, env, call_span)?);
                    }
                }
                Ok(Spanned::new(
                    Expr::Call(Box::new(new_func), new_args),
                    expr.span,
                ))
            }

            // Variable in quasiquote context - if it's a macro parameter, substitute
            Expr::Var(name) => {
                if let Some(replacement) = env.get(name) {
                    Ok(replacement.clone())
                } else {
                    Ok(expr.clone())
                }
            }

            // Literals - return as-is
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Keyword(_) => Ok(expr.clone()),

            // If expression - expand inside quasiquote
            Expr::If(cond, then_, else_) => {
                let new_cond = self.expand_quasiquote(cond, env, call_span)?;
                let new_then = self.expand_quasiquote(then_, env, call_span)?;
                let new_else = self.expand_quasiquote(else_, env, call_span)?;
                Ok(Spanned::new(
                    Expr::If(Box::new(new_cond), Box::new(new_then), Box::new(new_else)),
                    expr.span,
                ))
            }

            // Let bindings - expand inside quasiquote
            Expr::Let(bindings, body) => {
                let mut new_bindings = Vec::new();
                for binding in bindings {
                    let new_value = self.expand_quasiquote(&binding.value, env, call_span)?;
                    new_bindings.push(crate::ast::LetBinding {
                        name: binding.name.clone(),
                        ty: binding.ty.clone(),
                        value: new_value,
                    });
                }
                let new_body = self.expand_quasiquote(body, env, call_span)?;
                Ok(Spanned::new(
                    Expr::Let(new_bindings, Box::new(new_body)),
                    expr.span,
                ))
            }

            // Do expression - expand inside quasiquote
            Expr::Do(exprs) => {
                let new_exprs: Result<Vec<_>> = exprs
                    .iter()
                    .map(|e| self.expand_quasiquote(e, env, call_span))
                    .collect();
                Ok(Spanned::new(Expr::Do(new_exprs?), expr.span))
            }

            // Lambda - expand body inside quasiquote (but shadow params)
            Expr::Lambda(params, body) => {
                // Remove macro parameters that are shadowed by lambda parameters
                let mut new_env = env.clone();
                for param in params {
                    new_env.remove(&param.name.node);
                }
                let new_body = self.expand_quasiquote(body, &new_env, call_span)?;
                Ok(Spanned::new(
                    Expr::Lambda(params.clone(), Box::new(new_body)),
                    expr.span,
                ))
            }

            // For any other expressions, error - we need explicit handling
            _ => Err(CompileError::macro_error(
                expr.span,
                format!("unsupported expression in quasiquote: {:?}", expr.node),
            )),
        }
    }

    /// Remove pattern bindings from the environment (they shadow macro parameters)
    #[allow(clippy::only_used_in_recursion)]
    fn remove_pattern_bindings(
        &self,
        pattern: &crate::ast::Pattern,
        env: &mut HashMap<String, Spanned<Expr>>,
    ) {
        match pattern {
            crate::ast::Pattern::Var(name) => {
                env.remove(name);
            }
            crate::ast::Pattern::Struct(_, fields) => {
                for (_, pat) in fields {
                    self.remove_pattern_bindings(pat, env);
                }
            }
            crate::ast::Pattern::Tuple(patterns) => {
                for pat in patterns {
                    self.remove_pattern_bindings(pat, env);
                }
            }
            crate::ast::Pattern::Wildcard | crate::ast::Pattern::Literal(_) => {}
        }
    }
}

/// Expand all macros in a program
pub fn expand(program: &mut Program) -> Result<()> {
    let mut expander = Expander::new();
    expander.expand_program(program)
}
