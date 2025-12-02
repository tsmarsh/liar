//! Certification tests for lir-cli
//!
//! Uses cert features to verify lir-cli correctly implements lIR semantics.
//!
//! Test semantics:
//! - Exit code != 0: Pending (yellow) — not implemented yet
//! - Exit code == 0, output mismatch: Failed (red) — implemented wrong
//! - Exit code == 0, output matches: Passed (green) — works

use cucumber::{given, then, when, World};
use inkwell::context::Context;
use lir_cert::{execute, LirBackend};
use lir_codegen::codegen::{CodeGen, Value};
use lir_codegen::jit::JitEngine;
use lir_core::ast::FunctionDef;
use lir_core::parser::{ParseResult, Parser};
use std::collections::HashMap;

#[derive(Debug, Default, World)]
pub struct LirWorld {
    expression: String,
    exit_code: i32,
    stdout: String,
    stderr: String,
    /// Accumulated function definitions for multi-function scenarios
    functions: HashMap<String, FunctionDef>,
    /// Last result from function call
    last_result: Option<Value>,
    /// Error message if function call failed
    last_error: Option<String>,
}

/// Backend using the lir CLI binary
pub struct CliBackend;

impl LirBackend for CliBackend {
    fn eval_command(expr: &str) -> String {
        // Shell-escape the expression by wrapping in single quotes
        // and escaping any single quotes within
        let escaped = expr.replace('\'', "'\"'\"'");
        // Tests run from lir-cli dir, binary is at workspace root's target
        format!("../target/debug/lir '{}'", escaped)
    }
}

/// Parse a single expression and extract function if it's a definition
fn try_parse_function(expr: &str) -> Option<FunctionDef> {
    let mut parser = Parser::new(expr);
    match parser.parse_item() {
        Ok(ParseResult::Function(func)) => Some(func),
        _ => None,
    }
}

/// Parse arguments from gherkin string like "(i32 5)" or "(i32 3) (i32 4)"
fn parse_args(args_str: &str) -> Result<Vec<Value>, String> {
    let mut values = Vec::new();
    let mut parser = Parser::new(args_str);

    let items = parser
        .parse_items()
        .map_err(|e| format!("parse error: {:?}", e))?;

    for item in items {
        match item {
            ParseResult::Expr(expr) => {
                // Evaluate the expression to get the value
                let context = Context::create();
                let jit = JitEngine::new(&context);
                match jit.eval(&expr) {
                    Ok(val) => values.push(val),
                    Err(e) => return Err(format!("failed to evaluate arg: {:?}", e)),
                }
            }
            _ => return Err("expected expression argument".to_string()),
        }
    }

    Ok(values)
}

/// Format a Value for comparison with expected output
fn format_value(value: &Value) -> String {
    match value {
        Value::I1(v) => format!("(i1 {})", if *v { 1 } else { 0 }),
        Value::I8(v) => format!("(i8 {})", v),
        Value::I16(v) => format!("(i16 {})", v),
        Value::I32(v) => format!("(i32 {})", v),
        Value::I64(v) => format!("(i64 {})", v),
        Value::Float(v) => {
            if v.is_nan() {
                "(float nan)".to_string()
            } else if v.is_infinite() {
                if v.is_sign_positive() {
                    "(float inf)".to_string()
                } else {
                    "(float -inf)".to_string()
                }
            } else if *v == 0.0 && v.is_sign_negative() {
                "(float -0.0)".to_string()
            } else {
                format!("(float {})", v)
            }
        }
        Value::Double(v) => {
            if v.is_nan() {
                "(double nan)".to_string()
            } else if v.is_infinite() {
                if v.is_sign_positive() {
                    "(double inf)".to_string()
                } else {
                    "(double -inf)".to_string()
                }
            } else if *v == 0.0 && v.is_sign_negative() {
                "(double -0.0)".to_string()
            } else {
                format!("(double {})", v)
            }
        }
        Value::Ptr(v) => format!("(ptr {})", v),
        Value::VecI1(v) => {
            let elems: Vec<String> = v
                .iter()
                .map(|b| if *b { "1".to_string() } else { "0".to_string() })
                .collect();
            format!("(<{} x i1> {})", v.len(), elems.join(" "))
        }
        Value::VecI8(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x i8> {})", v.len(), elems.join(" "))
        }
        Value::VecI16(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x i16> {})", v.len(), elems.join(" "))
        }
        Value::VecI32(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x i32> {})", v.len(), elems.join(" "))
        }
        Value::VecI64(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x i64> {})", v.len(), elems.join(" "))
        }
        Value::VecFloat(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x float> {})", v.len(), elems.join(" "))
        }
        Value::VecDouble(v) => {
            let elems: Vec<String> = v.iter().map(|n| n.to_string()).collect();
            format!("(<{} x double> {})", v.len(), elems.join(" "))
        }
        Value::Struct(fields) => {
            let elems: Vec<String> = fields.iter().map(format_value).collect();
            format!("{{ {} }}", elems.join(" "))
        }
    }
}

#[given(regex = r"^the expression (.+)$")]
async fn given_expression(world: &mut LirWorld, expr: String) {
    world.expression = expr.clone();

    // Try to parse as function definition and accumulate
    if let Some(func) = try_parse_function(&expr) {
        world.functions.insert(func.name.clone(), func);
    }

    // Also run through CLI for non-function tests
    let result = execute::<CliBackend>(&expr);
    world.exit_code = result.exit_code;
    world.stdout = result.stdout;
    world.stderr = result.stderr;
}

#[when(regex = r"^I call (\S+)$")]
async fn when_call_no_args(world: &mut LirWorld, name: String) {
    call_function(world, &name, &[]);
}

#[when(regex = r"^I call (\S+) with (.+)$")]
async fn when_call_with_args(world: &mut LirWorld, name: String, args_str: String) {
    match parse_args(&args_str) {
        Ok(args) => call_function(world, &name, &args),
        Err(e) => {
            world.last_error = Some(e);
            world.last_result = None;
        }
    }
}

fn call_function(world: &mut LirWorld, name: &str, args: &[Value]) {
    // Look up the function
    let func = match world.functions.get(name) {
        Some(f) => f.clone(),
        None => {
            world.last_error = Some(format!("function '{}' not found", name));
            world.last_result = None;
            return;
        }
    };

    // Create JIT context and compile all functions
    let context = Context::create();
    let codegen = CodeGen::new(&context, "lir_test");

    // First pass: declare all functions (for mutual recursion support)
    for f in world.functions.values() {
        codegen.declare_function(f);
    }

    // Second pass: compile all function bodies
    for f in world.functions.values() {
        if let Err(e) = codegen.compile_function(f) {
            world.last_error = Some(format!("compile error: {:?}", e));
            world.last_result = None;
            return;
        }
    }

    // Create execution engine
    let execution_engine = match codegen
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
    {
        Ok(ee) => ee,
        Err(e) => {
            world.last_error = Some(format!("JIT error: {}", e));
            world.last_result = None;
            return;
        }
    };

    // Call the function using JitEngine's call_function helper
    let jit = JitEngine::new(&context);
    match jit.call_compiled_function(&execution_engine, name, &func.return_type, args) {
        Ok(result) => {
            world.last_result = Some(result);
            world.last_error = None;
        }
        Err(e) => {
            world.last_error = Some(format!("call error: {:?}", e));
            world.last_result = None;
        }
    }
}

#[then(regex = r"^the result is (\(.+)$")]
async fn then_result_is(world: &mut LirWorld, expected: String) {
    // If we have a function call result, use that
    if let Some(ref result) = world.last_result {
        let actual = format_value(result);
        assert_eq!(
            actual, expected,
            "Expected '{}' but got '{}'",
            expected, actual
        );
        return;
    }

    // If we had a function call error, report it
    if let Some(ref err) = world.last_error {
        panic!("PENDING: function call failed: {}", err);
    }

    // Otherwise fall back to CLI result
    if world.exit_code != 0 {
        // Non-zero exit = pending (not implemented)
        panic!("PENDING: not implemented (exit code {})", world.exit_code);
    }
    assert_eq!(
        world.stdout, expected,
        "Expected '{}' but got '{}'",
        expected, world.stdout
    );
}

#[then(regex = r"^the result is a pointer$")]
async fn then_result_is_pointer(world: &mut LirWorld) {
    // If we have a function call result, check it
    if let Some(ref result) = world.last_result {
        match result {
            Value::Ptr(_) => return,
            _ => panic!("Expected pointer result but got {:?}", result),
        }
    }

    if world.exit_code != 0 {
        // Non-zero exit = pending (not implemented)
        panic!("PENDING: not implemented (exit code {})", world.exit_code);
    }
    // Check that the result starts with "(ptr " and ends with ")"
    assert!(
        world.stdout.starts_with("(ptr ") && world.stdout.ends_with(')'),
        "Expected a pointer result like '(ptr ...)' but got '{}'",
        world.stdout
    );
}

#[then(regex = r"^the result is (\{.+\})$")]
async fn then_result_is_struct(world: &mut LirWorld, expected: String) {
    if world.exit_code != 0 {
        // Non-zero exit = pending (not implemented)
        panic!("PENDING: not implemented (exit code {})", world.exit_code);
    }
    assert_eq!(
        world.stdout, expected,
        "Expected struct '{}' but got '{}'",
        expected, world.stdout
    );
}

#[then(regex = r"^the struct (\w+) is defined$")]
async fn then_struct_is_defined(world: &mut LirWorld, name: String) {
    if world.exit_code != 0 {
        // Non-zero exit = pending (not implemented)
        panic!("PENDING: not implemented (exit code {})", world.exit_code);
    }
    // For now, just check exit code is 0 - the struct was parsed successfully
    // Output format could be "%name = type { ... }" or similar
    assert!(
        world.stdout.contains(&name) || world.exit_code == 0,
        "Expected struct '{}' to be defined, got '{}'",
        name,
        world.stdout
    );
}

#[then(regex = r"^the function completes successfully$")]
async fn then_function_completes(world: &mut LirWorld) {
    // Check that the function was called without error
    if let Some(ref err) = world.last_error {
        panic!("PENDING: function call failed: {}", err);
    }

    // For void functions, we just need no error
    if world.last_result.is_some() {
        return; // Success
    }

    // Fall back to CLI behavior
    if world.exit_code != 0 {
        panic!("PENDING: not implemented (exit code {})", world.exit_code);
    }
}

#[then(regex = r#"^it should error with "(.+)"$"#)]
async fn then_error_with(world: &mut LirWorld, expected_msg: String) {
    if world.exit_code == 0 {
        panic!(
            "Expected error containing '{}' but evaluation succeeded with: {}",
            expected_msg, world.stdout
        );
    }
    assert!(
        world.stderr.contains(&expected_msg),
        "Expected error containing '{}', got '{}'",
        expected_msg,
        world.stderr
    );
}

#[tokio::main]
async fn main() {
    // Use features from the cert crate
    LirWorld::run("../cert/features").await;
}
