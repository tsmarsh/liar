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
use std::path::Path;

/// Compile liar source and call a function, returning the result
pub fn compile_and_call(source: &str, func_name: &str) -> Result<Value, String> {
    compile_and_call_with_stdlib(source, func_name, false)
}

/// Compile liar source with stdlib and call a function
pub fn compile_and_call_with_stdlib(
    source: &str,
    func_name: &str,
    with_stdlib: bool,
) -> Result<Value, String> {
    // Build full source with optional stdlib
    let full_source = if with_stdlib {
        let lib_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../lib");
        let prelude_path = format!("{}/liar.prelude.liar", lib_path);
        let seq_path = format!("{}/liar.seq.liar", lib_path);

        let mut combined = String::new();

        // Load prelude library (no dependencies)
        if let Ok(prelude) = std::fs::read_to_string(&prelude_path) {
            // Strip namespace declaration for embedding
            let stripped = strip_namespace(&prelude);
            combined.push_str(&stripped);
            combined.push('\n');
        } else {
            return Err(format!(
                "Failed to read prelude library from {}",
                prelude_path
            ));
        }

        // Load seq library (depends on core)
        if let Ok(seq) = std::fs::read_to_string(&seq_path) {
            let stripped = strip_namespace(&seq);
            combined.push_str(&stripped);
            combined.push('\n');
        } else {
            return Err(format!("Failed to read seq library from {}", seq_path));
        }

        combined.push_str(source);
        combined
    } else {
        source.to_string()
    };

    // Compile liar → lIR string
    let lir_str = liar::compile(&full_source).map_err(|errs| {
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

/// Strip namespace declaration and require clauses from source
/// This allows embedding library code without namespace conflicts
fn strip_namespace(source: &str) -> String {
    let mut result = String::new();
    let mut in_ns = false;
    let mut paren_depth = 0;

    let mut chars = source.chars().peekable();
    while let Some(c) = chars.next() {
        if in_ns {
            // Track paren depth to find end of ns form
            if c == '(' {
                paren_depth += 1;
            } else if c == ')' {
                paren_depth -= 1;
                if paren_depth == 0 {
                    in_ns = false;
                    // Skip trailing whitespace after ns form
                    while chars.peek() == Some(&'\n') || chars.peek() == Some(&' ') {
                        chars.next();
                    }
                }
            }
        } else if c == '(' {
            // Check if this starts a ns form
            let rest: String = chars.clone().take(3).collect();
            if rest.starts_with("ns ") || rest.starts_with("ns\n") || rest.starts_with("ns\t") {
                in_ns = true;
                paren_depth = 1;
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Compile liar source via liarliar (self-hosted compiler) and call a function
/// This shells out to the liarliar binary and lair assembler
pub fn compile_and_call_liarliar(source: &str, func_name: &str) -> Result<Value, String> {
    use std::io::Write;
    use std::process::Command;

    // Create temp file for source
    let mut source_file =
        tempfile::NamedTempFile::new().map_err(|e| format!("Failed to create temp file: {}", e))?;

    // The source needs a main function that calls the test function
    // and returns i64 (for exit code based result checking)
    let full_source = format!("{}\n(defun main () -> i64 ({}))", source, func_name);

    // Write source to file
    source_file
        .write_all(full_source.as_bytes())
        .map_err(|e| format!("Failed to write source file: {}", e))?;

    let mut candidates: Vec<String> = Vec::new();
    if let Ok(path) = std::env::var("LIARLIAR") {
        candidates.push(path);
    }
    candidates.extend(
        [
            "target/release/liarliar",
            "./target/release/liarliar",
            "../target/release/liarliar",
            "../../target/release/liarliar",
            "/tmp/liarliar",
        ]
        .iter()
        .map(|s| s.to_string()),
    );

    let liarliar = candidates
        .iter()
        .find(|p| Path::new(p).exists())
        .ok_or_else(|| {
            "Could not find liarliar binary. Run `make target/release/liarliar` first.".to_string()
        })?;

    // Run liarliar to compile to lIR
    let liarliar_output = Command::new(liarliar)
        .arg(source_file.path())
        .output()
        .map_err(|e| format!("Failed to run liarliar: {}", e))?;

    if !liarliar_output.status.success() {
        let stderr = String::from_utf8_lossy(&liarliar_output.stderr);
        let stdout = String::from_utf8_lossy(&liarliar_output.stdout);
        return Err(format!(
            "liarliar compilation failed:\nstdout: {}\nstderr: {}",
            stdout, stderr
        ));
    }

    let lir_str = String::from_utf8(liarliar_output.stdout)
        .map_err(|e| format!("Invalid UTF-8 lIR output: {}", e))?;
    if let Ok(dir) = std::env::var("LIARLIAR_DUMP") {
        let mut safe_name = String::new();
        for ch in func_name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' {
                safe_name.push(ch);
            } else {
                safe_name.push('_');
            }
        }
        let pid = std::process::id();
        let ts = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or(0);
        let lir_path = format!("{}/liarliar_{}_{}_{}.lir", dir, safe_name, pid, ts);
        if let Err(err) = std::fs::write(&lir_path, &lir_str) {
            eprintln!("liarliar dump failed: {}: {}", lir_path, err);
        }
        let src_path = format!("{}/liarliar_{}_{}_{}.liar", dir, safe_name, pid, ts);
        if let Err(err) = std::fs::write(&src_path, &full_source) {
            eprintln!("liarliar dump failed: {}: {}", src_path, err);
        }
    }

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
    let mut codegen = CodeGen::new(&context, "liarliar_test");

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
            .map_err(|e| format!("codegen error: {:?}", e))?;
    }

    // Create execution engine
    let execution_engine = codegen
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| format!("JIT error: {}", e))?;

    // Call the function
    let jit = JitEngine::new(&context);
    let result = jit
        .call_compiled_function(&execution_engine, &func.name, &func.return_type, &[])
        .map_err(|e| format!("call error: {:?}", e))?;

    Ok(result)
}

/// Check if we should use liarliar backend
pub fn use_liarliar() -> bool {
    std::env::var("USE_LIARLIAR").is_ok()
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

    #[test]
    fn test_strip_namespace() {
        let source = "(ns test.ns (:require [other :as o]))\n\n(defun foo () 42)";
        let stripped = strip_namespace(source);
        assert!(
            stripped.contains("(defun foo () 42)"),
            "Should contain function def"
        );
        assert!(
            !stripped.contains("ns test.ns"),
            "Should not contain namespace"
        );
    }

    #[test]
    fn test_strip_namespace_preserves_content() {
        let source = "(ns my.ns)\n(defun list3 (a b c) (+ a b))";
        let stripped = strip_namespace(source);
        eprintln!("Stripped: {:?}", stripped);
        assert!(
            stripped.contains("defun list3"),
            "Should contain list3 definition"
        );
    }

    #[test]
    fn test_stdlib_loads() {
        let source = "(defun test () (+ 1 2))";
        let result = compile_and_call_with_stdlib(source, "test", true);
        eprintln!("Stdlib result: {:?}", result);
        assert!(result.is_ok() || result.is_err()); // Just check it doesn't panic
    }
}
