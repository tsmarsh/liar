//! Certification tests for liar
//!
//! Uses cucumber features to verify liar semantics.

use cucumber::{given, then, when, World};
use liar_cert::{compile_and_call, format_value};
use lir_codegen::codegen::Value;
use std::fs;

#[derive(Debug, Default, World)]
pub struct LiarWorld {
    /// Accumulated source (definitions)
    source: String,
    /// Function name to call
    func_name: String,
    /// Result of evaluation
    result: Option<Value>,
    /// Error message if compilation/evaluation failed
    error: Option<String>,
}

impl LiarWorld {
    fn call_function(&mut self, name: &str) {
        match compile_and_call(&self.source, name) {
            Ok(val) => {
                self.result = Some(val);
                self.error = None;
            }
            Err(e) => {
                self.result = None;
                self.error = Some(e);
            }
        }
    }
}

// --- Step Definitions ---

#[given("the standard library is loaded")]
async fn given_stdlib(world: &mut LiarWorld) {
    let stdlib_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../lib/stdlib.liar");
    if let Ok(stdlib) = fs::read_to_string(stdlib_path) {
        world.source = stdlib;
    }
}

#[given(regex = "^the definition \\(defun (.*)$")]
async fn given_definition(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(defun ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[when(regex = "^I evaluate \\(test\\)$")]
async fn when_evaluate_test(world: &mut LiarWorld) {
    world.func_name = "test".to_string();
    world.call_function("test");
}

#[then(regex = "^the result is (-?\\d+)$")]
async fn then_result_is(world: &mut LiarWorld, expected: String) {
    if let Some(ref err) = world.error {
        panic!("PENDING: {}", err);
    }

    let result = world.result.as_ref().expect("No result");
    let actual = format_value(result);

    assert_eq!(actual, expected, "Expected {} but got {}", expected, actual);
}

#[then(regex = "^the result is (true|false)$")]
async fn then_result_is_bool(world: &mut LiarWorld, expected: String) {
    if let Some(ref err) = world.error {
        panic!("PENDING: {}", err);
    }

    let result = world.result.as_ref().expect("No result");
    let actual = format_value(result);

    assert_eq!(actual, expected, "Expected {} but got {}", expected, actual);
}

#[tokio::main]
async fn main() {
    LiarWorld::run("features").await;
}
