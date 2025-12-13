//! Compile-time evaluator for macro expansion
//!
//! Macros are functions that evaluate at compile time and return AST.
//! This module provides the evaluator that runs macro bodies.

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::{Span, Spanned};

/// A compile-time value
#[derive(Clone, Debug)]
pub enum Value {
    /// Integer
    Int(i64),
    /// Float
    Float(f64),
    /// Boolean
    Bool(bool),
    /// String
    String(String),
    /// Symbol (quoted identifier)
    Symbol(String),
    /// Keyword
    Keyword(String),
    /// Nil
    Nil,
    /// List of values
    List(Vec<Value>),
    /// A closure (captured env + params + body)
    Closure {
        env: Env,
        params: Vec<String>,
        body: Spanned<Expr>,
    },
    /// An unevaluated expression (for quote)
    Expr(Spanned<Expr>),
}

impl Value {
    /// Try to unwrap Value::Expr containing a literal to its corresponding value
    /// This is needed because macro arguments are passed as expressions
    pub fn unwrap_literal(&self) -> Value {
        match self {
            Value::Expr(expr) => match &expr.node {
                Expr::Int(n) => Value::Int(*n),
                Expr::Float(f) => Value::Float(*f),
                Expr::Bool(b) => Value::Bool(*b),
                Expr::String(s) => Value::String(s.clone()),
                Expr::Nil => Value::Nil,
                Expr::Var(name) => Value::Symbol(name.clone()),
                Expr::Quote(s) => Value::Symbol(s.clone()),
                Expr::Keyword(s) => Value::Keyword(s.clone()),
                Expr::Vector(items) => {
                    // Recursively unwrap vector elements
                    let values: Vec<Value> = items
                        .iter()
                        .map(|item| Value::Expr(item.clone()).unwrap_literal())
                        .collect();
                    Value::List(values)
                }
                // Check if it's a (list ...) call expression
                Expr::Call(func, args) => {
                    if let Expr::Var(name) = &func.node {
                        if name == "list" {
                            let values: Vec<Value> = args
                                .iter()
                                .map(|item| Value::Expr(item.clone()).unwrap_literal())
                                .collect();
                            return Value::List(values);
                        }
                    }
                    // Otherwise keep as Expr
                    self.clone()
                }
                _ => self.clone(),
            },
            _ => self.clone(),
        }
    }

    /// Convert a value back to an expression (for macro return)
    pub fn to_expr(&self, span: Span) -> Spanned<Expr> {
        match self {
            Value::Int(n) => Spanned::new(Expr::Int(*n), span),
            Value::Float(f) => Spanned::new(Expr::Float(*f), span),
            Value::Bool(b) => Spanned::new(Expr::Bool(*b), span),
            Value::String(s) => Spanned::new(Expr::String(s.clone()), span),
            // Symbols become variable references (identifiers) in code context
            // This is correct for gensym usage in let bindings, function names, etc.
            Value::Symbol(s) => Spanned::new(Expr::Var(s.clone()), span),
            Value::Keyword(s) => Spanned::new(Expr::Keyword(s.clone()), span),
            Value::Nil => Spanned::new(Expr::Nil, span),
            Value::List(items) => {
                // Convert list to a vector expression
                let exprs: Vec<Spanned<Expr>> = items.iter().map(|v| v.to_expr(span)).collect();
                Spanned::new(Expr::Vector(exprs), span)
            }
            Value::Closure { .. } => {
                // Closures can't be converted back to expressions meaningfully
                Spanned::new(Expr::Nil, span)
            }
            Value::Expr(e) => e.clone(),
        }
    }

    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Nil)
    }
}

/// Environment for compile-time evaluation
#[derive(Clone, Debug, Default)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_parent(parent: Rc<Env>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn bind(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }
}

/// Struct info for reflection
#[derive(Clone, Debug)]
pub struct StructInfo {
    pub fields: Vec<(String, String)>, // (name, type)
}

/// Compile-time evaluator
#[derive(Default)]
pub struct Evaluator {
    structs: HashMap<String, StructInfo>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a struct for reflection
    pub fn register_struct(&mut self, name: String, info: StructInfo) {
        self.structs.insert(name, info);
    }

    /// Check if a struct is registered
    pub fn has_struct(&self, name: &str) -> bool {
        self.structs.contains_key(name)
    }

    /// Get struct fields
    pub fn get_struct_fields(&self, name: &str) -> Option<&[(String, String)]> {
        self.structs.get(name).map(|info| info.fields.as_slice())
    }

    /// Evaluate an expression at compile time
    pub fn eval(&self, env: &Env, expr: &Spanned<Expr>) -> Result<Value> {
        match &expr.node {
            // Literals
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Nil => Ok(Value::Nil),
            Expr::Keyword(k) => Ok(Value::Keyword(k.clone())),

            // Variables
            Expr::Var(name) => env.lookup(name).ok_or_else(|| {
                CompileError::macro_error(expr.span, format!("undefined variable '{}'", name))
            }),

            // Quote - return the expression as a symbol or expr
            Expr::Quote(s) => Ok(Value::Symbol(s.clone())),

            // Let binding
            Expr::Let(bindings, body) => {
                let mut new_env = Env::with_parent(Rc::new(env.clone()));
                for binding in bindings {
                    let value = self.eval(&new_env, &binding.value)?;
                    new_env.bind(binding.name.node.clone(), value);
                }
                self.eval(&new_env, body)
            }

            // If expression
            Expr::If(cond, then_branch, else_branch) => {
                let cond_val = self.eval(env, cond)?;
                if cond_val.is_truthy() {
                    self.eval(env, then_branch)
                } else {
                    self.eval(env, else_branch)
                }
            }

            // Lambda - create a closure
            Expr::Lambda(params, body) => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.node.clone()).collect();
                Ok(Value::Closure {
                    env: env.clone(),
                    params: param_names,
                    body: (**body).clone(),
                })
            }

            // Function call
            Expr::Call(func, args) => {
                // Check for built-in functions first
                if let Expr::Var(name) = &func.node {
                    if let Some(result) = self.eval_builtin(env, name, args, expr.span)? {
                        return Ok(result);
                    }
                }

                // Evaluate function and arguments
                let func_val = self.eval(env, func)?;
                let arg_vals: Result<Vec<Value>> = args.iter().map(|a| self.eval(env, a)).collect();
                let arg_vals = arg_vals?;

                self.apply(func_val, arg_vals, expr.span)
            }

            // Quasiquote - template with unquotes
            Expr::Quasiquote(inner) => self.eval_quasiquote(env, inner, expr.span),

            // Unquote - should only appear inside quasiquote
            Expr::Unquote(_) => Err(CompileError::macro_error(
                expr.span,
                "unquote outside of quasiquote",
            )),

            // Unquote-splicing - should only appear inside quasiquote
            Expr::UnquoteSplicing(_) => Err(CompileError::macro_error(
                expr.span,
                "unquote-splicing outside of quasiquote",
            )),

            // Do block - evaluate expressions in sequence
            Expr::Do(exprs) => {
                let mut result = Value::Nil;
                for e in exprs {
                    result = self.eval(env, e)?;
                }
                Ok(result)
            }

            // Vector literal - evaluate to a list
            Expr::Vector(items) => {
                let values: Result<Vec<Value>> = items.iter().map(|e| self.eval(env, e)).collect();
                Ok(Value::List(values?))
            }

            // Field access - can't evaluate at compile time normally
            Expr::Field(_, _) => Err(CompileError::macro_error(
                expr.span,
                "field access not supported at compile time",
            )),

            // Struct constructor - return as expression
            Expr::Struct(name, fields) => {
                // Evaluate field values and return as an Expr
                let mut eval_fields = Vec::new();
                for (fname, fval) in fields {
                    let val = self.eval(env, fval)?;
                    eval_fields.push((fname.clone(), val.to_expr(fval.span)));
                }
                Ok(Value::Expr(Spanned::new(
                    Expr::Struct(name.clone(), eval_fields),
                    expr.span,
                )))
            }

            // Other expressions - wrap as Expr value
            _ => Ok(Value::Expr(expr.clone())),
        }
    }

    /// Evaluate built-in functions
    fn eval_builtin(
        &self,
        env: &Env,
        name: &str,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Option<Value>> {
        match name {
            // Arithmetic
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
                        _ => {
                            return Err(CompileError::macro_error(span, "+ requires integers"));
                        }
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

            // Comparison
            "=" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "= requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                Ok(Some(Value::Bool(self.values_equal(&a, &b))))
            }
            "<" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "< requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => Ok(Some(Value::Bool(x < y))),
                    _ => Err(CompileError::macro_error(span, "< requires integers")),
                }
            }
            ">" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "> requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => Ok(Some(Value::Bool(x > y))),
                    _ => Err(CompileError::macro_error(span, "> requires integers")),
                }
            }
            "<=" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "<= requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => Ok(Some(Value::Bool(x <= y))),
                    _ => Err(CompileError::macro_error(span, "<= requires integers")),
                }
            }
            ">=" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, ">= requires 2 arguments"));
                }
                let a = self.eval(env, &args[0])?.unwrap_literal();
                let b = self.eval(env, &args[1])?.unwrap_literal();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => Ok(Some(Value::Bool(x >= y))),
                    _ => Err(CompileError::macro_error(span, ">= requires integers")),
                }
            }

            // Boolean
            "not" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "not requires 1 argument"));
                }
                let v = self.eval(env, &args[0])?;
                Ok(Some(Value::Bool(!v.is_truthy())))
            }
            "and" => {
                for arg in args {
                    let v = self.eval(env, arg)?;
                    if !v.is_truthy() {
                        return Ok(Some(Value::Bool(false)));
                    }
                }
                Ok(Some(Value::Bool(true)))
            }
            "or" => {
                for arg in args {
                    let v = self.eval(env, arg)?;
                    if v.is_truthy() {
                        return Ok(Some(Value::Bool(true)));
                    }
                }
                Ok(Some(Value::Bool(false)))
            }

            // List operations
            "list" => {
                let vals: Result<Vec<Value>> = args.iter().map(|a| self.eval(env, a)).collect();
                Ok(Some(Value::List(vals?)))
            }
            "cons" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "cons requires 2 arguments"));
                }
                let head = self.eval(env, &args[0])?;
                let tail = self.eval(env, &args[1])?;
                match tail {
                    Value::List(mut items) => {
                        items.insert(0, head);
                        Ok(Some(Value::List(items)))
                    }
                    Value::Nil => Ok(Some(Value::List(vec![head]))),
                    _ => Err(CompileError::macro_error(
                        span,
                        "cons requires a list as second argument",
                    )),
                }
            }
            "first" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "first requires 1 argument"));
                }
                let list = self.eval(env, &args[0])?;
                match list {
                    Value::List(items) => Ok(Some(items.first().cloned().unwrap_or(Value::Nil))),
                    Value::Nil => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "first requires a list")),
                }
            }
            "rest" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "rest requires 1 argument"));
                }
                let list = self.eval(env, &args[0])?;
                match list {
                    Value::List(items) => {
                        if items.is_empty() {
                            Ok(Some(Value::Nil))
                        } else {
                            Ok(Some(Value::List(items[1..].to_vec())))
                        }
                    }
                    Value::Nil => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "rest requires a list")),
                }
            }
            "nil?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "nil? requires 1 argument"));
                }
                let v = self.eval(env, &args[0])?;
                let is_nil = match v {
                    Value::Nil => true,
                    Value::List(ref items) if items.is_empty() => true,
                    _ => false,
                };
                Ok(Some(Value::Bool(is_nil)))
            }
            "empty?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "empty? requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                let is_empty = match v {
                    Value::Nil => true,
                    Value::List(items) => items.is_empty(),
                    Value::String(s) => s.is_empty(),
                    _ => false,
                };
                Ok(Some(Value::Bool(is_empty)))
            }
            "length" | "count" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "length requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                match v {
                    Value::List(items) => Ok(Some(Value::Int(items.len() as i64))),
                    Value::String(s) => Ok(Some(Value::Int(s.len() as i64))),
                    Value::Nil => Ok(Some(Value::Int(0))),
                    _ => Err(CompileError::macro_error(span, "length requires a list")),
                }
            }
            "append" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(
                        span,
                        "append requires 2 arguments",
                    ));
                }
                let a = self.eval(env, &args[0])?;
                let b = self.eval(env, &args[1])?;
                match (a, b) {
                    (Value::List(mut items1), Value::List(items2)) => {
                        items1.extend(items2);
                        Ok(Some(Value::List(items1)))
                    }
                    (Value::Nil, Value::List(items)) => Ok(Some(Value::List(items))),
                    (Value::List(items), Value::Nil) => Ok(Some(Value::List(items))),
                    (Value::Nil, Value::Nil) => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "append requires lists")),
                }
            }
            "reverse" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "reverse requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                match v {
                    Value::List(mut items) => {
                        items.reverse();
                        Ok(Some(Value::List(items)))
                    }
                    Value::Nil => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "reverse requires a list")),
                }
            }

            // Higher-order functions
            "map" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(span, "map requires 2 arguments"));
                }
                let func = self.eval(env, &args[0])?;
                let list = self.eval(env, &args[1])?;
                match list {
                    Value::List(items) => {
                        let mut results = Vec::new();
                        for item in items {
                            let result = self.apply(func.clone(), vec![item], span)?;
                            results.push(result);
                        }
                        Ok(Some(Value::List(results)))
                    }
                    Value::Nil => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "map requires a list")),
                }
            }
            "filter" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(
                        span,
                        "filter requires 2 arguments",
                    ));
                }
                let func = self.eval(env, &args[0])?;
                let list = self.eval(env, &args[1])?;
                match list {
                    Value::List(items) => {
                        let mut results = Vec::new();
                        for item in items {
                            let pred = self.apply(func.clone(), vec![item.clone()], span)?;
                            if pred.is_truthy() {
                                results.push(item);
                            }
                        }
                        Ok(Some(Value::List(results)))
                    }
                    Value::Nil => Ok(Some(Value::Nil)),
                    _ => Err(CompileError::macro_error(span, "filter requires a list")),
                }
            }
            "reduce" => {
                if args.len() != 3 {
                    return Err(CompileError::macro_error(
                        span,
                        "reduce requires 3 arguments",
                    ));
                }
                let func = self.eval(env, &args[0])?;
                let init = self.eval(env, &args[1])?;
                let list = self.eval(env, &args[2])?;
                match list {
                    Value::List(items) => {
                        let mut acc = init;
                        for item in items {
                            acc = self.apply(func.clone(), vec![acc, item], span)?;
                        }
                        Ok(Some(acc))
                    }
                    Value::Nil => Ok(Some(init)),
                    _ => Err(CompileError::macro_error(span, "reduce requires a list")),
                }
            }

            // String operations
            "str" => {
                let vals: Result<Vec<Value>> = args.iter().map(|a| self.eval(env, a)).collect();
                let vals = vals?;
                let mut result = String::new();
                for v in vals {
                    result.push_str(&self.value_to_string(&v));
                }
                Ok(Some(Value::String(result)))
            }
            "symbol" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "symbol requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                match v {
                    Value::String(s) => Ok(Some(Value::Symbol(s))),
                    Value::Symbol(s) => Ok(Some(Value::Symbol(s))),
                    _ => Err(CompileError::macro_error(
                        span,
                        "symbol requires a string or symbol",
                    )),
                }
            }
            "keyword" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "keyword requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                match v {
                    Value::String(s) => Ok(Some(Value::Keyword(s))),
                    Value::Symbol(s) => Ok(Some(Value::Keyword(s))),
                    Value::Keyword(s) => Ok(Some(Value::Keyword(s))),
                    _ => Err(CompileError::macro_error(
                        span,
                        "keyword requires a string or symbol",
                    )),
                }
            }

            // Type predicates
            "symbol?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "symbol? requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                Ok(Some(Value::Bool(matches!(v, Value::Symbol(_)))))
            }
            "keyword?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "keyword? requires 1 argument",
                    ));
                }
                let v = self.eval(env, &args[0])?;
                Ok(Some(Value::Bool(matches!(v, Value::Keyword(_)))))
            }
            "list?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "list? requires 1 argument"));
                }
                let v = self.eval(env, &args[0])?;
                Ok(Some(Value::Bool(matches!(v, Value::List(_)))))
            }
            "int?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(span, "int? requires 1 argument"));
                }
                let v = self.eval(env, &args[0])?;
                Ok(Some(Value::Bool(matches!(v, Value::Int(_)))))
            }

            // Reflection intrinsics
            "struct-fields" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "struct-fields requires 1 argument",
                    ));
                }
                let struct_name = self.extract_name(env, &args[0])?;
                let fields = self.get_struct_fields(&struct_name).ok_or_else(|| {
                    CompileError::macro_error(
                        span,
                        format!("unknown struct '{}' (not yet defined?)", struct_name),
                    )
                })?;
                let field_names: Vec<Value> = fields
                    .iter()
                    .map(|(name, _)| Value::Symbol(name.clone()))
                    .collect();
                Ok(Some(Value::List(field_names)))
            }
            "struct-field-type" => {
                if args.len() != 2 {
                    return Err(CompileError::macro_error(
                        span,
                        "struct-field-type requires 2 arguments",
                    ));
                }
                let struct_name = self.extract_name(env, &args[0])?;
                let field_name = self.extract_name(env, &args[1])?;
                let fields = self.get_struct_fields(&struct_name).ok_or_else(|| {
                    CompileError::macro_error(span, format!("unknown struct '{}'", struct_name))
                })?;
                let (_, ty) = fields
                    .iter()
                    .find(|(n, _)| n == &field_name)
                    .ok_or_else(|| {
                        CompileError::macro_error(
                            span,
                            format!("struct '{}' has no field '{}'", struct_name, field_name),
                        )
                    })?;
                Ok(Some(Value::Symbol(ty.clone())))
            }
            "struct?" => {
                if args.len() != 1 {
                    return Err(CompileError::macro_error(
                        span,
                        "struct? requires 1 argument",
                    ));
                }
                let name = self.extract_name(env, &args[0])?;
                Ok(Some(Value::Bool(self.has_struct(&name))))
            }

            // Gensym
            "gensym" => {
                use std::sync::atomic::{AtomicU64, Ordering};
                static COUNTER: AtomicU64 = AtomicU64::new(0);
                let n = COUNTER.fetch_add(1, Ordering::SeqCst);
                let prefix = if args.is_empty() {
                    "G__".to_string()
                } else {
                    let v = self.eval(env, &args[0])?;
                    match v {
                        Value::String(s) => format!("{}_", s),
                        Value::Symbol(s) => format!("{}_", s),
                        _ => "G__".to_string(),
                    }
                };
                Ok(Some(Value::Symbol(format!("{}{}", prefix, n))))
            }

            // Not a built-in
            _ => Ok(None),
        }
    }

    /// Apply a function to arguments
    fn apply(&self, func: Value, args: Vec<Value>, span: Span) -> Result<Value> {
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
                let mut new_env = Env::with_parent(Rc::new(env));
                for (param, arg) in params.into_iter().zip(args) {
                    new_env.bind(param, arg);
                }
                self.eval(&new_env, &body)
            }
            _ => Err(CompileError::macro_error(span, "cannot call non-function")),
        }
    }

    /// Evaluate quasiquote
    fn eval_quasiquote(&self, env: &Env, expr: &Spanned<Expr>, span: Span) -> Result<Value> {
        match &expr.node {
            Expr::Unquote(inner) => {
                // Evaluate and convert to Expr
                let val = self.eval(env, inner)?;
                Ok(Value::Expr(val.to_expr(inner.span)))
            }
            Expr::UnquoteSplicing(_) => {
                // Splicing is handled by the parent list processing
                Err(CompileError::macro_error(
                    span,
                    "unquote-splicing not in list context",
                ))
            }
            Expr::Call(func, args) => {
                // Process call, handling splicing in args
                let mut new_args = Vec::new();
                for arg in args {
                    if let Expr::UnquoteSplicing(inner) = &arg.node {
                        // Evaluate and splice
                        let val = self.eval(env, inner)?.unwrap_literal();
                        match val {
                            Value::List(items) => {
                                for item in items {
                                    new_args.push(item.to_expr(inner.span));
                                }
                            }
                            Value::Nil => {}
                            _ => {
                                return Err(CompileError::macro_error(
                                    span,
                                    "unquote-splicing requires a list",
                                ))
                            }
                        }
                    } else {
                        let result = self.eval_quasiquote(env, arg, span)?;
                        new_args.push(result.to_expr(arg.span));
                    }
                }
                let func_result = self.eval_quasiquote(env, func, span)?;
                Ok(Value::Expr(Spanned::new(
                    Expr::Call(Box::new(func_result.to_expr(func.span)), new_args),
                    span,
                )))
            }
            Expr::Vector(items) => {
                // Process vector, handling splicing
                let mut new_items = Vec::new();
                for item in items {
                    if let Expr::UnquoteSplicing(inner) = &item.node {
                        let val = self.eval(env, inner)?.unwrap_literal();
                        match val {
                            Value::List(list_items) => {
                                for v in list_items {
                                    new_items.push(v.to_expr(inner.span));
                                }
                            }
                            Value::Nil => {}
                            _ => {
                                return Err(CompileError::macro_error(
                                    span,
                                    "unquote-splicing requires a list",
                                ))
                            }
                        }
                    } else {
                        let result = self.eval_quasiquote(env, item, span)?;
                        new_items.push(result.to_expr(item.span));
                    }
                }
                Ok(Value::Expr(Spanned::new(Expr::Vector(new_items), span)))
            }
            Expr::Let(bindings, body) => {
                // Process let bindings in quasiquote
                let mut new_bindings = Vec::new();
                for binding in bindings {
                    let val_result = self.eval_quasiquote(env, &binding.value, span)?;
                    new_bindings.push(crate::ast::LetBinding {
                        name: binding.name.clone(),
                        ty: binding.ty.clone(),
                        value: val_result.to_expr(binding.value.span),
                    });
                }
                let body_result = self.eval_quasiquote(env, body, span)?;
                Ok(Value::Expr(Spanned::new(
                    Expr::Let(new_bindings, Box::new(body_result.to_expr(body.span))),
                    span,
                )))
            }
            Expr::If(cond, then_b, else_b) => {
                let cond_r = self.eval_quasiquote(env, cond, span)?;
                let then_r = self.eval_quasiquote(env, then_b, span)?;
                let else_r = self.eval_quasiquote(env, else_b, span)?;
                Ok(Value::Expr(Spanned::new(
                    Expr::If(
                        Box::new(cond_r.to_expr(cond.span)),
                        Box::new(then_r.to_expr(then_b.span)),
                        Box::new(else_r.to_expr(else_b.span)),
                    ),
                    span,
                )))
            }
            // For other expressions, just wrap as Expr
            _ => Ok(Value::Expr(expr.clone())),
        }
    }

    /// Check if two values are equal
    #[allow(clippy::only_used_in_recursion)]
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Symbol(x), Value::Symbol(y)) => x == y,
            (Value::Keyword(x), Value::Keyword(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::List(x), Value::List(y)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| self.values_equal(a, b))
            }
            _ => false,
        }
    }

    /// Convert a value to a string representation
    #[allow(clippy::only_used_in_recursion)]
    fn value_to_string(&self, v: &Value) -> String {
        match v {
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Symbol(s) => s.clone(),
            Value::Keyword(k) => format!(":{}", k),
            Value::Nil => "nil".to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| self.value_to_string(v)).collect();
                format!("({})", inner.join(" "))
            }
            Value::Closure { .. } => "#<closure>".to_string(),
            Value::Expr(_) => "#<expr>".to_string(),
        }
    }

    /// Extract a name from an expression (for reflection intrinsics)
    fn extract_name(&self, env: &Env, expr: &Spanned<Expr>) -> Result<String> {
        match &expr.node {
            // Quoted symbol
            Expr::Quote(name) => Ok(name.clone()),
            // Variable reference - look up and extract the name
            Expr::Var(name) => {
                if let Some(val) = env.lookup(name) {
                    self.extract_name_from_value(&val, expr.span)
                } else {
                    // Treat as a direct name (could be a struct name like Point)
                    Ok(name.clone())
                }
            }
            _ => {
                // Try evaluating it
                let val = self.eval(env, expr)?;
                self.extract_name_from_value(&val, expr.span)
            }
        }
    }

    /// Extract a name from a value
    fn extract_name_from_value(&self, val: &Value, span: Span) -> Result<String> {
        match val {
            Value::Symbol(s) => Ok(s.clone()),
            Value::String(s) => Ok(s.clone()),
            // Unwrap Value::Expr containing a variable/identifier
            Value::Expr(e) => match &e.node {
                Expr::Var(name) => Ok(name.clone()),
                Expr::Quote(name) => Ok(name.clone()),
                _ => Err(CompileError::macro_error(span, "expected a name")),
            },
            _ => Err(CompileError::macro_error(span, "expected a name")),
        }
    }
}
