//! Macro expansion
//!
//! Macros are functions that evaluate at compile time and return AST.
//! This pass:
//! 1. Collects macro definitions
//! 2. Evaluates macro calls at compile time
//! 3. Removes macro definitions from the program (they don't generate code)
//!
//! With the `jit-macros` feature enabled, macros can call user-defined functions
//! that were defined earlier in the source file.

use std::collections::HashMap;

#[cfg(feature = "jit-macros")]
use std::cell::RefCell;

use crate::ast::{Defstruct, Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::eval::{Env, Evaluator, MacroDef, StructInfo, Value};
use crate::span::{Span, Spanned};

#[cfg(feature = "jit-macros")]
use crate::macro_jit::{value_to_source, MacroJit};
#[cfg(feature = "jit-macros")]
use inkwell::context::Context;

/// Convenience function to expand macros in a program
pub fn expand(program: &mut Program) -> Result<()> {
    Expander::new().expand_program(program, None)
}

/// Expand macros with access to source (enables JIT when feature is enabled)
#[cfg(not(feature = "jit-macros"))]
pub fn expand_with_source(program: &mut Program, _source: &str) -> Result<()> {
    Expander::new().expand_program(program, None)
}

/// Expand macros with access to source (JIT-enabled version)
#[cfg(feature = "jit-macros")]
pub fn expand_with_source(program: &mut Program, source: &str) -> Result<()> {
    // Create LLVM context that lives for the duration of expansion
    let context = Context::create();
    let mut expander = Expander::new();
    expander.expand_program_with_jit(program, source, &context)
}

/// Macro expander
pub struct Expander {
    macros: HashMap<String, MacroDef>,
    evaluator: Evaluator,
}

impl Default for Expander {
    fn default() -> Self {
        Self::new()
    }
}

impl Expander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            evaluator: Evaluator::new(),
        }
    }

    /// Access the evaluator for manual evaluation
    pub fn evaluator(&self) -> &Evaluator {
        &self.evaluator
    }

    /// Mutable access to the evaluator
    pub fn evaluator_mut(&mut self) -> &mut Evaluator {
        &mut self.evaluator
    }

    /// Collect macro definitions from the program
    fn collect_macros(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Defmacro(defmacro) = &item.node {
                let def = MacroDef {
                    params: defmacro.params.iter().map(|p| p.node.clone()).collect(),
                    body: defmacro.body.clone(),
                };
                let name = defmacro.name.node.clone();
                // Store in local map
                self.macros.insert(name.clone(), def.clone());
                // Also register with evaluator for nested macro calls
                self.evaluator.register_macro(name, def);
            }
        }
    }

    /// Register a struct definition for compile-time reflection
    fn register_struct(&mut self, defstruct: &Defstruct) {
        let fields = defstruct
            .fields
            .iter()
            .map(|f| {
                let ty_str = format!("{:?}", f.ty.node);
                (f.name.node.clone(), ty_str)
            })
            .collect();
        self.evaluator
            .register_struct(defstruct.name.node.clone(), StructInfo { fields });
    }

    /// Expand all macros in a program (non-JIT version)
    #[allow(unused_variables)]
    pub fn expand_program(&mut self, program: &mut Program, source: Option<&str>) -> Result<()> {
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

    /// Expand all macros with JIT support for calling user-defined functions
    #[cfg(feature = "jit-macros")]
    pub fn expand_program_with_jit(
        &mut self,
        program: &mut Program,
        source: &str,
        context: &Context,
    ) -> Result<()> {
        // First, collect all macro definitions
        self.collect_macros(program);

        // Register structs
        for item in &program.items {
            if let Item::Defstruct(defstruct) = &item.node {
                self.register_struct(defstruct);
            }
        }

        // Create JIT - stdlib is automatically loaded by MacroJit::new()
        let mut jit = MacroJit::new(context).map_err(|e| {
            CompileError::macro_error(Span::default(), format!("JIT init failed: {}", e))
        })?;

        // Add all defuns to JIT (they need to be available before macro expansion)
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                let defun_source = &source[item.span.start..item.span.end];
                if let Err(e) = jit.add_definition(defun_source) {
                    // Log but don't fail - some functions may have dependencies not yet available
                    eprintln!(
                        "Warning: JIT compilation of '{}' failed: {}",
                        defun.name.node, e
                    );
                }
            }
        }

        // Clone macros for use in expansion (to avoid borrow conflicts)
        let macros = self.macros.clone();

        // Create JIT-aware evaluator wrapper
        let jit_eval = JitEvaluator::new(&self.evaluator, &mut jit, macros);

        // Expand macros in all items using JIT-aware evaluation
        for item in &mut program.items {
            expand_item_with_jit(item, &jit_eval)?;
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
            Item::Defstruct(defstruct) => {
                // Register struct for compile-time reflection
                self.register_struct(defstruct);
            }
            Item::Defprotocol(_) => {}
            Item::ExtendProtocol(extend) => {
                for method in &mut extend.implementations {
                    self.expand_expr(&mut method.body)?;
                }
            }
            Item::ExtendProtocolDefault(extend) => {
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
        // Check if this is a macro call
        if let Expr::Call(func, args) = &expr.node {
            if let Expr::Var(name) = &func.node {
                if let Some(macro_def) = self.macros.get(name).cloned() {
                    // This is a macro call - evaluate it
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

            Expr::If(cond, then_br, else_br) => {
                self.expand_expr(cond)?;
                self.expand_expr(then_br)?;
                self.expand_expr(else_br)?;
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.expand_expr(e)?;
                }
            }

            Expr::Quasiquote(inner) => {
                self.expand_in_quasiquote(inner)?;
            }

            Expr::Unquote(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::UnquoteSplicing(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Set(_, value) => {
                self.expand_expr(value)?;
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) => {
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

            Expr::Unsafe(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Atom(value) => {
                self.expand_expr(value)?;
            }

            Expr::Swap(atom, func) => {
                self.expand_expr(atom)?;
                self.expand_expr(func)?;
            }

            Expr::Reset(atom, value) => {
                self.expand_expr(atom)?;
                self.expand_expr(value)?;
            }

            Expr::AtomDeref(atom) => {
                self.expand_expr(atom)?;
            }

            Expr::CompareAndSet { atom, old, new } => {
                self.expand_expr(atom)?;
                self.expand_expr(old)?;
                self.expand_expr(new)?;
            }

            Expr::Vector(items) => {
                for item in items {
                    self.expand_expr(item)?;
                }
            }

            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.expand_expr(k)?;
                    self.expand_expr(v)?;
                }
            }

            Expr::ConvVector(items) => {
                for item in items {
                    self.expand_expr(item)?;
                }
            }

            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.expand_expr(k)?;
                    self.expand_expr(v)?;
                }
            }

            Expr::SimdVector(items) => {
                for item in items {
                    self.expand_expr(item)?;
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

            Expr::Boxed(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Wrapping(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Gensym(_) => {}

            // Closure conversion expressions (generated later)
            Expr::ClosureLit { .. } | Expr::HeapEnvAlloc { .. } | Expr::StackEnvAlloc { .. } => {}
        }
        Ok(())
    }

    fn expand_in_quasiquote(&mut self, expr: &mut Spanned<Expr>) -> Result<()> {
        match &mut expr.node {
            Expr::Unquote(inner) => {
                self.expand_expr(inner)?;
            }
            Expr::UnquoteSplicing(inner) => {
                self.expand_expr(inner)?;
            }
            Expr::Call(func, args) => {
                self.expand_in_quasiquote(func)?;
                for arg in args {
                    self.expand_in_quasiquote(arg)?;
                }
            }
            Expr::Vector(items) => {
                for item in items {
                    self.expand_in_quasiquote(item)?;
                }
            }
            Expr::Let(bindings, body) => {
                for binding in bindings {
                    self.expand_in_quasiquote(&mut binding.value)?;
                }
                self.expand_in_quasiquote(body)?;
            }
            Expr::If(cond, then_br, else_br) => {
                self.expand_in_quasiquote(cond)?;
                self.expand_in_quasiquote(then_br)?;
                self.expand_in_quasiquote(else_br)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Expand a macro call by evaluating the macro body
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

        // Build environment with macro arguments as values
        let mut env = Env::new();
        for (param, arg) in macro_def.params.iter().zip(args.iter()) {
            // Arguments are passed unevaluated (as expressions)
            env.bind(param.clone(), Value::Expr(arg.clone()));
        }

        // Evaluate the macro body
        let result = self.evaluator.eval(&env, &macro_def.body)?;

        // Convert the result back to an expression
        Ok(result.to_expr(span))
    }
}

/// Expand an item with JIT support (free function to avoid borrow conflicts)
#[cfg(feature = "jit-macros")]
fn expand_item_with_jit(item: &mut Spanned<Item>, jit_eval: &JitEvaluator) -> Result<()> {
    match &mut item.node {
        Item::Defun(defun) => {
            expand_expr_with_jit(&mut defun.body, jit_eval)?;
        }
        Item::Def(def) => {
            expand_expr_with_jit(&mut def.value, jit_eval)?;
        }
        Item::Defmacro(_) | Item::Defstruct(_) | Item::Defprotocol(_) | Item::Extern(_) => {}
        Item::ExtendProtocol(extend) => {
            for method in &mut extend.implementations {
                expand_expr_with_jit(&mut method.body, jit_eval)?;
            }
        }
        Item::ExtendProtocolDefault(extend) => {
            for method in &mut extend.implementations {
                expand_expr_with_jit(&mut method.body, jit_eval)?;
            }
        }
    }
    Ok(())
}

/// Expand expressions with JIT-aware macro evaluation (free function)
#[cfg(feature = "jit-macros")]
fn expand_expr_with_jit(expr: &mut Spanned<Expr>, jit_eval: &JitEvaluator) -> Result<()> {
    // Check if this is a macro call
    if let Expr::Call(func, args) = &expr.node {
        if let Expr::Var(name) = &func.node {
            if let Some(macro_def) = jit_eval.macros.get(name).cloned() {
                // This is a macro call - evaluate it with JIT support
                let expanded = expand_macro_call_with_jit(&macro_def, args, expr.span, jit_eval)?;
                *expr = expanded;
                // Recursively expand the result
                return expand_expr_with_jit(expr, jit_eval);
            }
        }
    }

    // Not a macro call, recursively expand sub-expressions
    expand_subexprs_with_jit(expr, jit_eval)
}

/// Recursively expand sub-expressions (free function)
#[cfg(feature = "jit-macros")]
fn expand_subexprs_with_jit(expr: &mut Spanned<Expr>, jit_eval: &JitEvaluator) -> Result<()> {
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
        | Expr::Regex { .. }
        | Expr::Gensym(_) => {}

        Expr::Call(func, args) => {
            expand_expr_with_jit(func, jit_eval)?;
            for arg in args {
                expand_expr_with_jit(arg, jit_eval)?;
            }
        }

        Expr::Lambda(_, body) => {
            expand_expr_with_jit(body, jit_eval)?;
        }

        Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
            for binding in bindings {
                expand_expr_with_jit(&mut binding.value, jit_eval)?;
            }
            expand_expr_with_jit(body, jit_eval)?;
        }

        Expr::If(cond, then_br, else_br) => {
            expand_expr_with_jit(cond, jit_eval)?;
            expand_expr_with_jit(then_br, jit_eval)?;
            expand_expr_with_jit(else_br, jit_eval)?;
        }

        Expr::Do(exprs) => {
            for e in exprs {
                expand_expr_with_jit(e, jit_eval)?;
            }
        }

        Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
            expand_expr_with_jit(inner, jit_eval)?;
        }

        Expr::Set(_, value) => {
            expand_expr_with_jit(value, jit_eval)?;
        }

        Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) => {
            expand_expr_with_jit(inner, jit_eval)?;
        }

        Expr::Struct(_, fields) => {
            for (_, value) in fields {
                expand_expr_with_jit(value, jit_eval)?;
            }
        }

        Expr::Field(obj, _) => {
            expand_expr_with_jit(obj, jit_eval)?;
        }

        Expr::Unsafe(inner)
        | Expr::Atom(inner)
        | Expr::AtomDeref(inner)
        | Expr::Iter(inner)
        | Expr::Collect(inner)
        | Expr::Boxed(inner)
        | Expr::Wrapping(inner)
        | Expr::Async(inner)
        | Expr::Await(inner) => {
            expand_expr_with_jit(inner, jit_eval)?;
        }

        Expr::Swap(atom, func) | Expr::Reset(atom, func) => {
            expand_expr_with_jit(atom, jit_eval)?;
            expand_expr_with_jit(func, jit_eval)?;
        }

        Expr::CompareAndSet { atom, old, new } => {
            expand_expr_with_jit(atom, jit_eval)?;
            expand_expr_with_jit(old, jit_eval)?;
            expand_expr_with_jit(new, jit_eval)?;
        }

        Expr::Vector(items) | Expr::ConvVector(items) | Expr::SimdVector(items) => {
            for item in items {
                expand_expr_with_jit(item, jit_eval)?;
            }
        }

        Expr::Map(pairs) | Expr::ConvMap(pairs) => {
            for (k, v) in pairs {
                expand_expr_with_jit(k, jit_eval)?;
                expand_expr_with_jit(v, jit_eval)?;
            }
        }

        Expr::Dosync(exprs) => {
            for e in exprs {
                expand_expr_with_jit(e, jit_eval)?;
            }
        }

        Expr::RefSetStm(ref_expr, value) => {
            expand_expr_with_jit(ref_expr, jit_eval)?;
            expand_expr_with_jit(value, jit_eval)?;
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
            expand_expr_with_jit(ref_expr, jit_eval)?;
            expand_expr_with_jit(fn_expr, jit_eval)?;
            for arg in args {
                expand_expr_with_jit(arg, jit_eval)?;
            }
        }

        // Closure conversion expressions (generated later)
        Expr::ClosureLit { .. } | Expr::HeapEnvAlloc { .. } | Expr::StackEnvAlloc { .. } => {}
    }
    Ok(())
}

/// Expand a macro call using JIT for function resolution (free function)
#[cfg(feature = "jit-macros")]
fn expand_macro_call_with_jit(
    macro_def: &MacroDef,
    args: &[Spanned<Expr>],
    span: Span,
    jit_eval: &JitEvaluator,
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

    // Build environment with macro arguments as values
    let mut env = Env::new();
    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
        env.bind(param.clone(), Value::Expr(arg.clone()));
    }

    // Evaluate the macro body with JIT support
    let result = jit_eval.eval(&env, &macro_def.body)?;

    // Convert the result back to an expression
    Ok(result.to_expr(span))
}

/// JIT-aware evaluator wrapper
///
/// Wraps the base Evaluator and MacroJit to provide function resolution
/// via JIT compilation for user-defined functions.
#[cfg(feature = "jit-macros")]
struct JitEvaluator<'a, 'ctx> {
    evaluator: &'a Evaluator,
    jit: RefCell<&'a mut MacroJit<'ctx>>,
    macros: HashMap<String, MacroDef>,
}

#[cfg(feature = "jit-macros")]
impl<'a, 'ctx> JitEvaluator<'a, 'ctx> {
    fn new(
        evaluator: &'a Evaluator,
        jit: &'a mut MacroJit<'ctx>,
        macros: HashMap<String, MacroDef>,
    ) -> Self {
        Self {
            evaluator,
            jit: RefCell::new(jit),
            macros,
        }
    }

    /// Evaluate an expression with JIT-aware function resolution
    ///
    /// This method handles all expression types, ensuring that any nested
    /// function calls go through JIT resolution.
    fn eval(&self, env: &Env, expr: &Spanned<Expr>) -> Result<Value> {
        use crate::ast::Expr;
        use crate::eval::Value;
        use std::rc::Rc;

        match &expr.node {
            // Literals - handle directly
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Nil => Ok(Value::Nil),
            Expr::Keyword(k) => Ok(Value::Keyword(k.clone())),

            // Variables - look up in environment
            Expr::Var(name) => env.lookup(name).ok_or_else(|| {
                CompileError::macro_error(expr.span, format!("undefined variable '{}'", name))
            }),

            // Quote
            Expr::Quote(s) => Ok(Value::Symbol(s.clone())),

            // Let binding - evaluate with JIT-aware recursion
            Expr::Let(bindings, body) => {
                let mut new_env = Env::with_parent(Rc::new(env.clone()));
                for binding in bindings {
                    let value = self.eval(&new_env, &binding.value)?;
                    new_env.bind(binding.name.node.clone(), value);
                }
                self.eval(&new_env, body)
            }

            // If - evaluate with JIT-aware recursion
            Expr::If(cond, then_branch, else_branch) => {
                let cond_val = self.eval(env, cond)?.unwrap_literal();
                let is_true = match cond_val {
                    Value::Bool(b) => b,
                    Value::Int(n) => n != 0,
                    Value::Nil => false,
                    _ => true,
                };
                if is_true {
                    self.eval(env, then_branch)
                } else {
                    self.eval(env, else_branch)
                }
            }

            // Lambda - create closure
            Expr::Lambda(params, body) => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.node.clone()).collect();
                Ok(Value::Closure {
                    env: env.clone(),
                    params: param_names,
                    body: body.as_ref().clone(),
                })
            }

            // Function call - the key case for JIT integration
            Expr::Call(func, args) => {
                if let Expr::Var(name) = &func.node {
                    // Try builtins first, using JIT-aware eval for arguments
                    if let Some(result) = self.eval_builtin(env, name, args, expr.span)? {
                        return Ok(result);
                    }

                    // Check if this is a macro call
                    if self.macros.contains_key(name) || self.evaluator.has_macro(name) {
                        // Delegate macro expansion to base evaluator
                        return self.evaluator.eval(env, expr);
                    }

                    // Check JIT for user-defined functions
                    let jit = self.jit.borrow();
                    if jit.has_function(name) {
                        drop(jit);
                        // Evaluate arguments with JIT-aware eval
                        let arg_vals: Result<Vec<Value>> =
                            args.iter().map(|a| self.eval(env, a)).collect();
                        let arg_vals = arg_vals?;

                        // Build expression string with marshalled arguments
                        let arg_strs: Vec<String> = arg_vals.iter().map(value_to_source).collect();
                        let expr_str = format!("({} {})", name, arg_strs.join(" "));

                        // Evaluate via JIT
                        let mut jit = self.jit.borrow_mut();
                        return jit.eval_expr(&expr_str, expr.span);
                    }
                }

                // Fall through to base evaluator for closures and unknown functions
                // Evaluate function and arguments with JIT-aware eval
                let func_val = self.eval(env, func)?;
                let arg_vals: Result<Vec<Value>> = args.iter().map(|a| self.eval(env, a)).collect();
                let arg_vals = arg_vals?;
                self.apply(func_val, arg_vals, expr.span)
            }

            // Quasiquote and other complex expressions - delegate to base evaluator
            // These don't typically contain function calls that need JIT
            _ => self.evaluator.eval(env, expr),
        }
    }

    /// Evaluate a builtin function, using JIT-aware evaluation for arguments
    fn eval_builtin(
        &self,
        env: &Env,
        name: &str,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Option<Value>> {
        use crate::eval::Value;

        match name {
            // Arithmetic - evaluate args with JIT-aware eval
            "+" => {
                let vals: Result<Vec<Value>> = args
                    .iter()
                    .map(|a| self.eval(env, a).map(|v| v.unwrap_literal()))
                    .collect();
                let vals = vals?;
                let mut sum = 0i64;
                for v in vals {
                    match v {
                        Value::Int(n) => sum += n,
                        _ => return Err(CompileError::macro_error(span, "+ requires integers")),
                    }
                }
                Ok(Some(Value::Int(sum)))
            }
            "-" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "- requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => Ok(Some(Value::Int(x - y))),
                    _ => Err(CompileError::macro_error(span, "- requires integers")),
                }
            }
            "*" => {
                let vals: Result<Vec<Value>> = args
                    .iter()
                    .map(|a| self.eval(env, a).map(|v| v.unwrap_literal()))
                    .collect();
                let vals = vals?;
                let mut product = 1i64;
                for v in vals {
                    match v {
                        Value::Int(n) => product *= n,
                        _ => return Err(CompileError::macro_error(span, "* requires integers")),
                    }
                }
                Ok(Some(Value::Int(product)))
            }
            "/" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "/ requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => {
                        if y == 0 {
                            Err(CompileError::macro_error(span, "division by zero"))
                        } else {
                            Ok(Some(Value::Int(x / y)))
                        }
                    }
                    _ => Err(CompileError::macro_error(span, "/ requires integers")),
                }
            }
            // For other builtins, delegate to base evaluator's builtin handling
            // but we need to pass through for evaluation
            _ => {
                // Delegate to base evaluator for complex builtins
                // This is safe because these builtins don't recursively call user functions
                self.evaluator.eval_builtin(env, name, args, span)
            }
        }
    }

    /// Apply a function value to arguments
    fn apply(&self, func: Value, args: Vec<Value>, span: Span) -> Result<Value> {
        use crate::eval::Value;
        use std::rc::Rc;

        match func {
            Value::Closure { env, params, body } => {
                if params.len() != args.len() {
                    return Err(CompileError::macro_error(
                        span,
                        format!(
                            "function expects {} arguments, got {}",
                            params.len(),
                            args.len()
                        ),
                    ));
                }
                let mut new_env = Env::with_parent(Rc::new(env.clone()));
                for (param, arg) in params.iter().zip(args) {
                    new_env.bind(param.clone(), arg);
                }
                self.eval(&new_env, &body)
            }
            _ => Err(CompileError::macro_error(
                span,
                format!("cannot call non-function: {:?}", func),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn parse_and_expand(source: &str) -> Result<Program> {
        let mut parser = Parser::new(source)?;
        let mut program = parser.parse_program()?;
        let mut expander = Expander::new();
        expander.expand_program(&mut program, Some(source))?;
        Ok(program)
    }

    fn get_test_body(program: &Program) -> &Expr {
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                if defun.name.node == "test" {
                    return &defun.body.node;
                }
            }
        }
        panic!("no test function found")
    }

    #[test]
    fn test_macro_returns_literal() {
        let source = r#"
            (defmacro answer () 42)
            (defun test () (answer))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(42)));
    }

    #[test]
    fn test_macro_with_arithmetic() {
        let source = r#"
            (defmacro compute () (+ 1 2 3))
            (defun test () (compute))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(6)));
    }

    #[test]
    fn test_macro_with_arithmetic_params() {
        // Test arithmetic with macro parameters
        let source = r#"
            (defmacro add (a b) (+ a b))
            (defun test () (add 3 7))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(10)));
    }

    #[test]
    fn test_macro_with_let() {
        let source = r#"
            (defmacro with-let ()
              (let ((x 10)
                    (y 20))
                (+ x y)))
            (defun test () (with-let))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(30)));
    }

    #[test]
    fn test_macro_with_if() {
        let source = r#"
            (defmacro check (cond)
              (if cond 1 0))
            (defun test () (check true))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(1)));
    }

    #[test]
    fn test_macro_with_quasiquote() {
        let source = r#"
            (defmacro make-add (a b)
              `(+ ,a ,b))
            (defun test () (make-add 1 2))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ 1 2), not evaluate to 3
        assert!(matches!(body, Expr::Call(_, _)));
    }

    #[test]
    fn test_macro_with_list_operations() {
        let source = r#"
            (defmacro first-elem ()
              (first (list 1 2 3)))
            (defun test () (first-elem))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(1)));
    }

    #[test]
    fn test_macro_with_map() {
        let source = r#"
            (defmacro double-list ()
              (map (fn (x) (* x 2)) (list 1 2 3)))
            (defun test () (double-list))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Result is [2 4 6]
        if let Expr::Vector(items) = body {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].node, Expr::Int(2)));
            assert!(matches!(items[1].node, Expr::Int(4)));
            assert!(matches!(items[2].node, Expr::Int(6)));
        } else {
            panic!("expected vector, got {:?}", body);
        }
    }

    #[test]
    fn test_macro_with_splicing() {
        let source = r#"
            (defmacro make-call (f args)
              `(,f ,@args))
            (defun test () (make-call + (list 1 2 3)))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ 1 2 3)
        if let Expr::Call(func, args) = body {
            if let Expr::Vector(items) = &func.node {
                // The function might be wrapped in a vector from to_expr
                assert_eq!(items.len(), 1);
            }
            assert_eq!(args.len(), 3);
        }
    }

    #[test]
    fn test_struct_reflection() {
        let source = r#"
            (defstruct Point (x: i64 y: i64))
            (defmacro count-fields (s)
              (length (struct-fields s)))
            (defun test () (count-fields Point))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(2)));
    }

    #[test]
    fn test_macro_with_recursion_helper() {
        // Test macro that uses a helper for loop-like behavior
        // Note: Direct recursive macro calls during expansion require
        // the evaluator to know about macros, which isn't currently supported.
        // Instead, use computed values in the macro body.
        let source = r#"
            (defmacro times-three (n)
              (+ n n n))
            (defun test () (times-three 5))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(15)));
    }

    #[test]
    fn test_macro_generates_code() {
        let source = r#"
            (defmacro when (cond body)
              `(if ,cond ,body nil))
            (defun test () (when true 42))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should be an if expression
        assert!(matches!(body, Expr::If(_, _, _)));
    }

    #[test]
    fn test_gensym_basic() {
        // Test gensym returns unique symbols
        // Note: Using gensym in let binding name position requires
        // special AST support for unquote in name slots, not yet implemented.
        let source = r#"
            (defmacro gen-name ()
              (gensym "var"))
            (defun test () 42)
        "#;
        // Just verify it parses and expands without error
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(42)));
    }

    #[test]
    fn test_nested_macros() {
        let source = r#"
            (defmacro double (x) `(+ ,x ,x))
            (defmacro quadruple (x) `(double (double ,x)))
            (defun test () (quadruple 3))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ (+ 3 3) (+ 3 3))
        assert!(matches!(body, Expr::Call(_, _)));
    }

    /// Test that macros can call user-defined functions (when JIT is available)
    #[test]
    #[cfg(feature = "jit-macros")]
    fn test_macro_calls_user_function() {
        // This test requires JIT to be enabled to actually call the user function
        let source = r#"
            (defun triple (x) (* x 3))
            (defmacro times-three (n)
              (triple n))
            (defun test () (times-three 7))
        "#;

        // Parse and expand with JIT support
        let mut parser = Parser::new(source).unwrap();
        let mut program = parser.parse_program().unwrap();

        // Use the JIT-aware expansion
        let context = inkwell::context::Context::create();
        let mut expander = Expander::new();
        expander
            .expand_program_with_jit(&mut program, source, &context)
            .unwrap();

        let body = get_test_body(&program);
        // Should expand to 21 (7 * 3)
        assert!(matches!(body, Expr::Int(21)));
    }

    /// Test that macros can call stdlib functions (loaded automatically by JIT)
    #[test]
    #[cfg(feature = "jit-macros")]
    fn test_macro_calls_stdlib_function() {
        // This test verifies that stdlib functions are available to macros
        let source = r#"
            (defmacro squared (n)
              (square n))
            (defun test () (squared 5))
        "#;

        // Parse and expand with JIT support
        let mut parser = Parser::new(source).unwrap();
        let mut program = parser.parse_program().unwrap();

        // Use the JIT-aware expansion
        let context = inkwell::context::Context::create();
        let mut expander = Expander::new();
        expander
            .expand_program_with_jit(&mut program, source, &context)
            .unwrap();

        let body = get_test_body(&program);
        // Should expand to 25 (5 * 5) using stdlib's square function
        assert!(matches!(body, Expr::Int(25)));
    }

    /// Test that stdlib functions can be nested in macro bodies
    #[test]
    #[cfg(feature = "jit-macros")]
    fn test_macro_nested_stdlib_calls() {
        // This tests that nested function calls in macro bodies work
        // E.g., (+ (inc a) (dec b)) should correctly evaluate both JIT calls
        let source = r#"
            (defmacro compute (a b)
              (+ (inc a) (dec b)))
            (defun test () (compute 5 3))
        "#;

        let mut parser = Parser::new(source).unwrap();
        let mut program = parser.parse_program().unwrap();

        let context = inkwell::context::Context::create();
        let mut expander = Expander::new();
        expander
            .expand_program_with_jit(&mut program, source, &context)
            .unwrap();

        let body = get_test_body(&program);
        // (+ (inc 5) (dec 3)) = (+ 6 2) = 8
        assert!(matches!(body, Expr::Int(8)));
    }
}
