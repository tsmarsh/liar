//! JIT-powered macro evaluation
//!
//! This module provides JIT compilation for macro bodies, allowing macros
//! to call any liar function that's been defined before them.
//!
//! The stdlib is automatically loaded when the JIT is created, so macros
//! can use all stdlib functions without any special handling.
//!
//! Requires the `jit-macros` feature.

use inkwell::context::Context;
use lir_codegen::codegen::Value as JitValue;
use lir_codegen::IncrementalJit;
use lir_core::parser::{ParseResult, Parser};
use std::collections::HashSet;

use crate::error::{CompileError, Result};
use crate::eval::Value;
use crate::span::Span;

/// Embedded stdlib source - loaded automatically by MacroJit
const STDLIB_SOURCE: &str = include_str!("../../lib/liar.core.liar");

/// JIT context for macro evaluation
///
/// Lazily initialized, lives for the duration of compilation.
/// Functions are compiled incrementally as they're defined.
pub struct MacroJit<'ctx> {
    /// The LLVM context (kept for future use)
    #[allow(dead_code)]
    context: &'ctx Context,
    /// Incremental JIT engine
    jit: IncrementalJit<'ctx>,
    /// Functions already compiled for macro use
    available_functions: HashSet<String>,
    /// Accumulated liar source for compiled functions
    definitions_source: String,
    /// Counter for unique eval thunk names
    eval_counter: u64,
}

impl<'ctx> MacroJit<'ctx> {
    /// Create a new macro JIT context with stdlib pre-loaded
    ///
    /// The stdlib is automatically compiled and available for macros to use.
    pub fn new(context: &'ctx Context) -> Result<Self> {
        let jit = IncrementalJit::new(context)
            .map_err(|e| CompileError::macro_error(Span::default(), format!("JIT init: {}", e)))?;

        let mut instance = Self {
            context,
            jit,
            available_functions: HashSet::new(),
            definitions_source: String::new(),
            eval_counter: 0,
        };

        // Bootstrap stdlib - strip macros which aren't supported yet in JIT
        instance.bootstrap_stdlib()?;

        Ok(instance)
    }

    /// Bootstrap the stdlib into the JIT
    ///
    /// This compiles the stdlib functions so they're available to macros.
    /// Functions that can't be JIT compiled (macros, closures) are skipped.
    fn bootstrap_stdlib(&mut self) -> Result<()> {
        // Filter out macros and closure-returning functions from stdlib
        // Macros are handled by the Rust evaluator
        // Closure-returning functions (constantly, comp, etc.) can't be JIT compiled
        // because the JIT doesn't have the closure environment struct types
        let filtered = filter_macros(STDLIB_SOURCE);
        let filtered = filter_closure_functions(&filtered);
        if !filtered.is_empty() {
            // Try to add definitions - skip on error since some may have unsupported features
            if let Err(_e) = self.add_definition(&filtered) {
                // Fall back to adding functions one by one to isolate failures
                self.add_definitions_individually(&filtered)?;
            }
        }
        Ok(())
    }

    /// Add definitions one by one, skipping any that fail
    fn add_definitions_individually(&mut self, source: &str) -> Result<()> {
        // Parse source to extract top-level forms
        let mut parser = crate::parser::Parser::new(source).map_err(|e| {
            CompileError::macro_error(Span::default(), format!("Parse error: {}", e))
        })?;
        let program = parser.parse_program().map_err(|e| {
            CompileError::macro_error(Span::default(), format!("Parse error: {}", e))
        })?;

        for item in &program.items {
            if let crate::ast::Item::Defun(defun) = &item.node {
                let defun_source = &source[item.span.start..item.span.end];
                // Try to add, ignore failures
                if let Err(_e) = self.add_definition(defun_source) {
                    // Skip this function - it can't be JIT compiled
                    eprintln!(
                        "Note: stdlib function '{}' not available in macros (closure or unsupported feature)",
                        defun.name.node
                    );
                }
            }
        }
        Ok(())
    }

    /// Add a function definition to the JIT
    ///
    /// The function becomes available for macros to call.
    pub fn add_definition(&mut self, source: &str) -> Result<Vec<String>> {
        // Compile liar → lIR string (including all previous definitions)
        let full_source = format!("{}\n{}", self.definitions_source, source);
        let lir_str = crate::compile(&full_source).map_err(|errs| {
            CompileError::macro_error(
                Span::default(),
                errs.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?;

        // Parse lIR → AST
        let mut parser = Parser::new(&lir_str);
        let items = parser.parse_items().map_err(|e| {
            CompileError::macro_error(Span::default(), format!("lIR parse error: {:?}", e))
        })?;

        // Collect new functions
        let mut new_functions = Vec::new();
        for item in &items {
            if let ParseResult::Function(func) = item {
                if !self.available_functions.contains(&func.name) {
                    new_functions.push(func);
                }
            }
        }

        if new_functions.is_empty() {
            return Ok(vec![]);
        }

        // Add all new functions at once
        self.jit.define_functions(&new_functions).map_err(|e| {
            CompileError::macro_error(Span::default(), format!("JIT compile error: {:?}", e))
        })?;

        // Track the source and functions
        self.definitions_source.push_str(source);
        self.definitions_source.push('\n');

        let names: Vec<String> = new_functions.iter().map(|f| f.name.clone()).collect();
        for name in &names {
            self.available_functions.insert(name.clone());
        }

        Ok(names)
    }

    /// Check if a function is available in the JIT
    pub fn has_function(&self, name: &str) -> bool {
        self.available_functions.contains(name)
    }

    /// Evaluate an expression and return a macro Value
    ///
    /// The expression is wrapped in a thunk, compiled, executed, and
    /// the result is converted to a macro Value.
    pub fn eval_expr(&mut self, expr_source: &str, span: Span) -> Result<Value> {
        // Wrap in a thunk function
        self.eval_counter += 1;
        let thunk_name = format!("__macro_eval_{}", self.eval_counter);
        let full_source = format!(
            "{}\n(defun {} () {})",
            self.definitions_source, thunk_name, expr_source
        );

        // Compile liar → lIR
        let lir_str = crate::compile(&full_source).map_err(|errs| {
            CompileError::macro_error(
                span,
                errs.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?;

        // Parse lIR
        let mut parser = Parser::new(&lir_str);
        let items = parser
            .parse_items()
            .map_err(|e| CompileError::macro_error(span, format!("lIR parse error: {:?}", e)))?;

        // Find the thunk function
        let thunk = items
            .iter()
            .rev()
            .find_map(|item| {
                if let ParseResult::Function(func) = item {
                    if func.name == thunk_name {
                        return Some(func.clone());
                    }
                }
                None
            })
            .ok_or_else(|| {
                CompileError::macro_error(span, "failed to create eval thunk".to_string())
            })?;

        // Compile and execute the thunk
        let jit_value = self
            .jit
            .eval_thunk(&thunk)
            .map_err(|e| CompileError::macro_error(span, format!("JIT eval error: {:?}", e)))?;

        // Convert JIT value to macro Value
        self.jit_to_value(jit_value, span)
    }

    /// Convert a JIT value to a macro Value
    fn jit_to_value(&self, jit_value: JitValue, _span: Span) -> Result<Value> {
        match jit_value {
            JitValue::I1(b) => Ok(Value::Bool(b)),
            JitValue::I8(n) => Ok(Value::Int(n as i64)),
            JitValue::I16(n) => Ok(Value::Int(n as i64)),
            JitValue::I32(n) => Ok(Value::Int(n as i64)),
            JitValue::I64(n) => Ok(Value::Int(n)),
            JitValue::Float(f) => Ok(Value::Float(f as f64)),
            JitValue::Double(d) => Ok(Value::Float(d)),
            JitValue::Ptr(0) => Ok(Value::Nil),
            JitValue::Ptr(ptr) => {
                // Read the list structure from memory
                // This requires knowing the Cons cell layout
                unsafe { self.read_list_from_ptr(ptr) }
            }
            _ => Ok(Value::Nil), // Vectors and structs not yet supported
        }
    }

    /// Read a list from a pointer to a Cons cell
    ///
    /// # Safety
    /// The pointer must point to a valid Cons cell structure or be null.
    #[allow(clippy::only_used_in_recursion)]
    unsafe fn read_list_from_ptr(&self, ptr: u64) -> Result<Value> {
        if ptr == 0 {
            return Ok(Value::Nil);
        }

        // Cons cell layout: [type_id: i64, head: i64, tail: ptr]
        // type_id is at offset 0, head at offset 8, tail at offset 16
        let cell_ptr = ptr as *const u64;

        // Read type_id (should be 1 for Cons)
        let type_id = *cell_ptr;
        if type_id != 1 {
            // Not a Cons cell, return as opaque pointer
            return Ok(Value::Int(ptr as i64));
        }

        // Read head (i64 value)
        let head = *cell_ptr.add(1);

        // Read tail (pointer)
        let tail_ptr = *cell_ptr.add(2);

        // Recursively read tail
        let tail = self.read_list_from_ptr(tail_ptr)?;

        // Build the list
        match tail {
            Value::List(mut items) => {
                items.insert(0, Value::Int(head as i64));
                Ok(Value::List(items))
            }
            Value::Nil => Ok(Value::List(vec![Value::Int(head as i64)])),
            other => {
                // Improper list (cons pair with non-list tail)
                Ok(Value::List(vec![Value::Int(head as i64), other]))
            }
        }
    }
}

/// Convert a macro Value to liar source code
///
/// This is used to pass arguments to JIT-compiled functions.
pub fn value_to_source(value: &Value) -> String {
    match value {
        Value::Int(n) => format!("{}", n),
        Value::Float(f) => format!("{}", f),
        Value::Bool(true) => "true".to_string(),
        Value::Bool(false) => "false".to_string(),
        Value::String(s) => format!("{:?}", s),
        Value::Symbol(s) => format!("'{}", s),
        Value::Keyword(k) => format!(":{}", k),
        Value::Nil => "nil".to_string(),
        Value::List(items) => {
            if items.is_empty() {
                "nil".to_string()
            } else {
                let inner: Vec<String> = items.iter().map(value_to_source).collect();
                format!("(list {})", inner.join(" "))
            }
        }
        Value::Closure { .. } => {
            // Closures can't be passed to JIT - return nil
            "nil".to_string()
        }
        Value::Expr(e) => {
            // Convert expression to source - extract literal values
            expr_to_source(&e.node)
        }
    }
}

/// Convert an expression node to source code
fn expr_to_source(expr: &crate::ast::Expr) -> String {
    use crate::ast::Expr;
    match expr {
        Expr::Int(n) => format!("{}", n),
        Expr::Float(f) => format!("{}", f),
        Expr::Bool(true) => "true".to_string(),
        Expr::Bool(false) => "false".to_string(),
        Expr::String(s) => format!("{:?}", s),
        Expr::Nil => "nil".to_string(),
        Expr::Var(name) => name.to_string(),
        Expr::Quote(s) => format!("'{}", s),
        Expr::Keyword(k) => format!(":{}", k),
        Expr::Vector(items) => {
            if items.is_empty() {
                "nil".to_string()
            } else {
                let inner: Vec<String> = items.iter().map(|e| expr_to_source(&e.node)).collect();
                format!("(list {})", inner.join(" "))
            }
        }
        Expr::Call(func, args) => {
            let func_str = expr_to_source(&func.node);
            let args_str: Vec<String> = args.iter().map(|a| expr_to_source(&a.node)).collect();
            format!("({} {})", func_str, args_str.join(" "))
        }
        // For other expressions, fall back to debug format (may not be valid liar)
        _ => format!("{:?}", expr),
    }
}

/// Filter out closure-returning functions from source code
///
/// Functions that return closures (contain `(fn `) can't be JIT compiled
/// because the closure environment structs aren't registered.
fn filter_closure_functions(source: &str) -> String {
    let mut result = String::new();
    let mut depth = 0;
    let mut in_defun = false;
    let mut defun_depth = 0;
    #[allow(unused_assignments)]
    let mut _defun_start = 0;
    let mut defun_content = String::new();
    let mut i = 0;
    let chars: Vec<char> = source.chars().collect();

    while i < chars.len() {
        let c = chars[i];

        // Handle comments
        if c == ';' {
            if in_defun {
                defun_content.push(c);
            } else {
                result.push(c);
            }
            i += 1;
            while i < chars.len() && chars[i] != '\n' {
                if in_defun {
                    defun_content.push(chars[i]);
                } else {
                    result.push(chars[i]);
                }
                i += 1;
            }
            continue;
        }

        // Handle open paren
        if c == '(' {
            depth += 1;
            let remaining: String = chars[i..].iter().collect();
            if !in_defun && remaining.starts_with("(defun ") {
                in_defun = true;
                defun_depth = depth;
                _defun_start = i;
                defun_content.push(c);
                i += 1;
                continue;
            }
            if in_defun {
                defun_content.push(c);
            } else {
                result.push(c);
            }
            i += 1;
            continue;
        }

        // Handle close paren
        if c == ')' {
            if in_defun {
                defun_content.push(c);
                if depth == defun_depth {
                    in_defun = false;
                    // Check if this defun contains a closure
                    if !defun_content.contains("(fn ") && !defun_content.contains("(fn\n") {
                        result.push_str(&defun_content);
                    }
                    defun_content.clear();
                }
            } else {
                result.push(c);
            }
            depth -= 1;
            i += 1;
            continue;
        }

        // Handle other characters
        if in_defun {
            defun_content.push(c);
        } else {
            result.push(c);
        }
        i += 1;
    }

    // Handle any remaining defun content (malformed, but don't drop it)
    if !defun_content.is_empty() && !defun_content.contains("(fn ") {
        result.push_str(&defun_content);
    }

    result
}

/// Filter out macro definitions from source code
///
/// Macros can't be JIT compiled - they're handled by the Rust evaluator.
/// This does a simple text-based filter to skip (defmacro ...) forms.
fn filter_macros(source: &str) -> String {
    let mut result = String::new();
    let mut depth = 0;
    let mut in_defmacro = false;
    let mut defmacro_depth = 0;
    let mut i = 0;
    let chars: Vec<char> = source.chars().collect();

    while i < chars.len() {
        let c = chars[i];

        // Handle comments
        if c == ';' {
            if !in_defmacro {
                result.push(c);
            }
            i += 1;
            while i < chars.len() && chars[i] != '\n' {
                if !in_defmacro {
                    result.push(chars[i]);
                }
                i += 1;
            }
            continue;
        }

        // Handle open paren
        if c == '(' {
            depth += 1;
            // Look ahead for defmacro
            let remaining: String = chars[i..].iter().collect();
            if remaining.starts_with("(defmacro ") || remaining.starts_with("(defmacro\n") {
                in_defmacro = true;
                defmacro_depth = depth;
                i += 1;
                continue;
            }
            if !in_defmacro {
                result.push(c);
            }
            i += 1;
            continue;
        }

        // Handle close paren
        if c == ')' {
            if in_defmacro {
                if depth == defmacro_depth {
                    in_defmacro = false;
                }
            } else {
                result.push(c);
            }
            depth -= 1;
            i += 1;
            continue;
        }

        // Handle other characters
        if !in_defmacro {
            result.push(c);
        }
        i += 1;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_jit_creation() {
        let context = Context::create();
        let jit = MacroJit::new(&context);
        assert!(jit.is_ok());
    }

    #[test]
    fn test_add_simple_function() {
        let context = Context::create();
        let mut jit = MacroJit::new(&context).unwrap();

        let result = jit.add_definition("(defun twice (x) (* x 2))");
        if let Err(ref e) = result {
            eprintln!("Error: {}", e);
        }
        assert!(result.is_ok());
        assert!(jit.has_function("twice"));
    }

    #[test]
    fn test_eval_simple_expr() {
        let context = Context::create();
        let mut jit = MacroJit::new(&context).unwrap();

        // Add a function
        jit.add_definition("(defun triple (x) (* x 3))").unwrap();

        // Evaluate an expression using that function
        let result = jit.eval_expr("(triple 7)", Span::default());
        assert!(result.is_ok());
        if let Value::Int(n) = result.unwrap() {
            assert_eq!(n, 21);
        } else {
            panic!("expected Int");
        }
    }

    #[test]
    fn test_filter_macros() {
        let source = r#"
(defun foo (x) x)
(defmacro bar (y) y)
(defun baz (z) z)
"#;
        let filtered = filter_macros(source);
        assert!(filtered.contains("defun foo"));
        assert!(!filtered.contains("defmacro"));
        assert!(filtered.contains("defun baz"));
    }

    #[test]
    fn test_stdlib_functions_available() {
        let context = Context::create();
        let jit = MacroJit::new(&context).unwrap();

        // Check some stdlib functions are available
        assert!(jit.has_function("inc"));
        assert!(jit.has_function("dec"));
        assert!(jit.has_function("square"));
        assert!(jit.has_function("factorial"));
    }

    #[test]
    fn test_eval_stdlib_function() {
        let context = Context::create();
        let mut jit = MacroJit::new(&context).unwrap();

        // Use stdlib functions directly
        let result = jit.eval_expr("(square 5)", Span::default());
        assert!(result.is_ok());
        if let Value::Int(n) = result.unwrap() {
            assert_eq!(n, 25);
        } else {
            panic!("expected Int");
        }

        // Test inc
        let result = jit.eval_expr("(inc 10)", Span::default());
        assert!(result.is_ok());
        if let Value::Int(n) = result.unwrap() {
            assert_eq!(n, 11);
        } else {
            panic!("expected Int");
        }
    }
}
