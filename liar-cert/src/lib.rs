//! liar Test Harness
//!
//! Provides test infrastructure for liar cucumber tests.

use inkwell::context::Context;
use inkwell::OptimizationLevel;
use lir_codegen::codegen::{CodeGen, Value};
use lir_codegen::jit::JitEngine;
use lir_core::ast::{ExternDecl, FunctionDef, StructDef};
use lir_core::parser::{ParseResult, Parser};
use std::collections::HashMap;

/// Compile liar source and call a function, returning the result
pub fn compile_and_call(source: &str, func_name: &str) -> Result<Value, String> {
    // Compile liar → lIR string
    let lir_str = liar::compile(source).map_err(|errs| {
        errs.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Parse lIR string → AST items
    let mut parser = Parser::new(&lir_str);
    let items = parser
        .parse_items()
        .map_err(|e| format!("lIR parse error: {:?}", e))?;

    // Collect all functions, structs, and extern declarations
    let mut functions: HashMap<String, FunctionDef> = HashMap::new();
    let mut structs: Vec<StructDef> = Vec::new();
    let mut externs: Vec<ExternDecl> = Vec::new();
    for item in items {
        match item {
            ParseResult::Function(func) => {
                functions.insert(func.name.clone(), func);
            }
            ParseResult::Struct(s) => {
                structs.push(s);
            }
            ParseResult::ExternDecl(e) => {
                externs.push(e);
            }
            _ => {}
        }
    }

    // Find the target function
    let func = functions
        .get(func_name)
        .ok_or_else(|| format!("function '{}' not found", func_name))?
        .clone();

    // Create JIT context and compile all functions
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "liar_test");

    // Register struct types first (before compiling functions that use them)
    for s in &structs {
        codegen.register_struct_type(&s.name, &s.fields);
    }

    // Compile extern declarations (for malloc, etc.)
    for e in &externs {
        codegen
            .compile_extern_decl(e)
            .map_err(|e| format!("extern compile error: {:?}", e))?;
    }

    // First pass: declare all functions (for mutual recursion support)
    for f in functions.values() {
        codegen.declare_function(f);
    }

    // Second pass: compile all function bodies
    for f in functions.values() {
        codegen
            .compile_function(f)
            .map_err(|e| format!("compile error: {:?}", e))?;
    }

    // Create execution engine
    let execution_engine = codegen
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| format!("JIT error: {}", e))?;

    // Call the function
    let jit = JitEngine::new(&context);
    jit.call_compiled_function(&execution_engine, func_name, &func.return_type, &[])
        .map_err(|e| format!("call error: {:?}", e))
}

/// Format a Value for display
pub fn format_value(value: &Value) -> String {
    match value {
        Value::I1(b) => {
            if *b {
                "true"
            } else {
                "false"
            }
        }
        .to_string(),
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
    fn test_simple_add() {
        let source = "(defun test () (+ 1 2))";
        let result = compile_and_call(source, "test").unwrap();
        assert_eq!(format_value(&result), "3");
    }

    #[test]
    fn test_inc_function() {
        let source = r#"
            (defun inc (x) (+ x 1))
            (defun test () (inc 5))
        "#;
        let result = compile_and_call(source, "test").unwrap();
        assert_eq!(format_value(&result), "6");
    }
}
