//! Session management for REPL
//!
//! A session accumulates definitions and maintains JIT state for
//! interactive evaluation.

use inkwell::context::Context;
use lir_codegen::codegen::Value;
use lir_codegen::IncrementalJit;
use lir_core::ast::FunctionDef;
use lir_core::parser::{ParseResult, Parser};
use std::collections::HashMap;

/// Result of evaluating input
#[derive(Debug)]
pub enum EvalResult {
    /// A value was computed
    Value(Value),
    /// A definition was added
    Defined(String),
    /// An error occurred
    Error(String),
}

/// Metadata about a definition
#[derive(Debug, Clone)]
pub struct Definition {
    pub name: String,
    pub kind: DefKind,
    pub source: String,
}

/// Kind of definition
#[derive(Debug, Clone)]
pub enum DefKind {
    Function,
    Constant,
    Struct,
}

/// A REPL session with incremental compilation
pub struct Session<'ctx> {
    /// Session identifier
    id: String,
    /// The LLVM context (owned by the session)
    context: &'ctx Context,
    /// Incremental JIT engine
    jit: IncrementalJit<'ctx>,
    /// Accumulated liar source (for re-compilation if needed)
    definitions_source: String,
    /// Parsed definitions for lookup/completion
    definitions: HashMap<String, Definition>,
    /// Counter for unique eval thunk names
    eval_counter: u64,
}

impl<'ctx> Session<'ctx> {
    /// Create a new session
    pub fn new(context: &'ctx Context, id: String) -> Result<Self, String> {
        let jit = IncrementalJit::new(context)?;

        Ok(Self {
            id,
            context,
            jit,
            definitions_source: String::new(),
            definitions: HashMap::new(),
            eval_counter: 0,
        })
    }

    /// Get the session ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get the LLVM context
    pub fn context(&self) -> &'ctx Context {
        self.context
    }

    /// Evaluate input, returning result
    pub fn eval(&mut self, input: &str) -> EvalResult {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return EvalResult::Error("empty input".to_string());
        }

        // Check if this is a definition
        if self.is_definition(trimmed) {
            return self.add_definition(trimmed);
        }

        // It's an expression - evaluate it
        self.eval_expression(trimmed)
    }

    /// Check if input looks like a definition
    fn is_definition(&self, input: &str) -> bool {
        input.starts_with("(defun")
            || input.starts_with("(def ")
            || input.starts_with("(defstruct")
            || input.starts_with("(defprotocol")
    }

    /// Add a definition to the session
    fn add_definition(&mut self, source: &str) -> EvalResult {
        // Compile liar → lIR string
        let full_source = format!("{}\n{}", self.definitions_source, source);
        let lir_str = match liar::compile(&full_source) {
            Ok(s) => s,
            Err(errs) => {
                let msg = errs
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                return EvalResult::Error(msg);
            }
        };

        // Parse lIR → AST
        let mut parser = Parser::new(&lir_str);
        let items = match parser.parse_items() {
            Ok(items) => items,
            Err(e) => return EvalResult::Error(format!("lIR parse error: {:?}", e)),
        };

        // Collect functions
        let mut functions: Vec<FunctionDef> = Vec::new();
        for item in items {
            if let ParseResult::Function(func) = item {
                functions.push(func);
            }
        }

        // Find new functions (ones not already defined)
        let new_functions: Vec<&FunctionDef> = functions
            .iter()
            .filter(|f| !self.jit.has_symbol(&f.name))
            .collect();

        if new_functions.is_empty() {
            return EvalResult::Error("no new definitions found".to_string());
        }

        // Add all new functions at once (for mutual recursion support)
        if let Err(e) = self.jit.define_functions(&new_functions) {
            return EvalResult::Error(format!("compile error: {:?}", e));
        }

        // Track the source and definitions
        self.definitions_source.push_str(source);
        self.definitions_source.push('\n');

        // Extract names from the new definitions
        let names: Vec<String> = new_functions.iter().map(|f| f.name.clone()).collect();

        // Track in definitions map
        for func in &new_functions {
            self.definitions.insert(
                func.name.clone(),
                Definition {
                    name: func.name.clone(),
                    kind: DefKind::Function,
                    source: source.to_string(),
                },
            );
        }

        if names.len() == 1 {
            EvalResult::Defined(names[0].clone())
        } else {
            EvalResult::Defined(format!("({} definitions)", names.len()))
        }
    }

    /// Evaluate an expression
    fn eval_expression(&mut self, source: &str) -> EvalResult {
        // Compile the expression in context of existing definitions
        let full_source = format!(
            "{}\n(defun __repl_eval_{} () {})",
            self.definitions_source, self.eval_counter, source
        );
        self.eval_counter += 1;

        let lir_str = match liar::compile(&full_source) {
            Ok(s) => s,
            Err(errs) => {
                let msg = errs
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                return EvalResult::Error(msg);
            }
        };

        // Parse lIR
        let mut parser = Parser::new(&lir_str);
        let items = match parser.parse_items() {
            Ok(items) => items,
            Err(e) => return EvalResult::Error(format!("lIR parse error: {:?}", e)),
        };

        // Find the eval function (should be the last one)
        let eval_func = items.iter().rev().find_map(|item| {
            if let ParseResult::Function(func) = item {
                if func.name.starts_with("__repl_eval_") {
                    Some(func.clone())
                } else {
                    None
                }
            } else {
                None
            }
        });

        let func = match eval_func {
            Some(f) => f,
            None => return EvalResult::Error("failed to create eval thunk".to_string()),
        };

        // Compile and call the thunk
        match self.jit.eval_thunk(&func) {
            Ok(value) => EvalResult::Value(value),
            Err(e) => EvalResult::Error(format!("eval error: {:?}", e)),
        }
    }

    /// Get completions for a prefix
    pub fn completions(&self, prefix: &str) -> Vec<String> {
        self.definitions
            .keys()
            .filter(|name| name.starts_with(prefix))
            .cloned()
            .collect()
    }

    /// Look up a definition
    pub fn lookup(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    /// Get all definition names
    pub fn definition_names(&self) -> Vec<&str> {
        self.definitions.keys().map(|s| s.as_str()).collect()
    }
}

/// Format a Value for display
pub fn format_value(value: &Value) -> String {
    match value {
        Value::I1(b) => if *b { "true" } else { "false" }.to_string(),
        Value::I8(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I64(n) => n.to_string(),
        Value::Float(f) => {
            if f.fract() == 0.0 {
                format!("{:.1}", f)
            } else {
                format!("{}", f)
            }
        }
        Value::Double(d) => {
            if d.fract() == 0.0 {
                format!("{:.1}", d)
            } else {
                format!("{}", d)
            }
        }
        Value::Ptr(0) => "nil".to_string(),
        Value::Ptr(p) => format!("<ptr {:x}>", p),
        Value::VecI1(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|b| if *b { "true" } else { "false" })
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI8(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI16(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI32(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI64(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecFloat(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecDouble(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::Struct(fields) => {
            let parts: Vec<String> = fields.iter().map(format_value).collect();
            format!("{{{}}}", parts.join(" "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_expression() {
        let context = Context::create();
        let mut session = Session::new(&context, "test".to_string()).unwrap();

        match session.eval("(+ 1 2)") {
            EvalResult::Value(v) => assert_eq!(format_value(&v), "3"),
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    // TODO: Incremental JIT with function definitions returns wrong values on macOS.
    // Simple expressions work, but defining functions and calling them fails.
    // The cert tests (single-shot compilation) pass on macOS, suggesting an
    // issue with incremental module linking on macOS ARM.
    #[cfg_attr(target_os = "macos", ignore)]
    fn test_definition_and_call() {
        let context = Context::create();
        let mut session = Session::new(&context, "test".to_string()).unwrap();

        // Define a function
        match session.eval("(defun inc (x) (+ x 1))") {
            EvalResult::Defined(name) => {
                assert!(name.contains("inc") || name.contains("definition"))
            }
            other => panic!("expected Defined, got {:?}", other),
        }

        // Call it
        match session.eval("(inc 5)") {
            EvalResult::Value(v) => assert_eq!(format_value(&v), "6"),
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    // TODO: Same issue as test_definition_and_call - incremental JIT broken on macOS
    #[cfg_attr(target_os = "macos", ignore)]
    fn test_multiple_definitions() {
        let context = Context::create();
        let mut session = Session::new(&context, "test".to_string()).unwrap();

        // Define multiple functions
        assert!(matches!(
            session.eval("(defun twice (x) (* x 2))"),
            EvalResult::Defined(_)
        ));
        assert!(matches!(
            session.eval("(defun triple (x) (* x 3))"),
            EvalResult::Defined(_)
        ));

        // Call them
        match session.eval("(twice 5)") {
            EvalResult::Value(v) => assert_eq!(format_value(&v), "10"),
            other => panic!("expected Value, got {:?}", other),
        }

        match session.eval("(triple 5)") {
            EvalResult::Value(v) => assert_eq!(format_value(&v), "15"),
            other => panic!("expected Value, got {:?}", other),
        }
    }
}
