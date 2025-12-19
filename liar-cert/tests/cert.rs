//! Certification tests for liar
//!
//! Uses cucumber features to verify liar semantics.

use cucumber::runner::Basic;
use cucumber::{given, then, when, World};
use liar_cert::{
    compile_and_call, compile_and_call_liarliar, compile_and_call_with_stdlib, format_value,
    use_liarliar,
};
use lir_codegen::codegen::Value;

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
    /// Whether to load stdlib
    with_stdlib: bool,
}

impl LiarWorld {
    fn call_function(&mut self, name: &str) {
        let result = if use_liarliar() {
            // Use self-hosted liarliar compiler
            compile_and_call_liarliar(&self.source, name)
        } else if self.with_stdlib {
            compile_and_call_with_stdlib(&self.source, name, true)
        } else {
            compile_and_call(&self.source, name)
        };
        match result {
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
    world.with_stdlib = true;
}

#[given(regex = "^the definition \\(defun (.*)$")]
async fn given_definition(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(defun ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the definition \\(defstruct (.*)$")]
async fn given_defstruct(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(defstruct ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the definition \\(defmacro (.*)$")]
async fn given_defmacro(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(defmacro ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the definition \\(defprotocol (.*)$")]
async fn given_defprotocol(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(defprotocol ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the definition \\(extend-protocol (.*)$")]
async fn given_extend_protocol(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(extend-protocol ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the definition \\(extend-protocol-default (.*)$")]
async fn given_extend_protocol_default(world: &mut LiarWorld, rest: String) {
    world.source.push_str("(extend-protocol-default ");
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

#[then(regex = "^the float result is (-?\\d+\\.\\d+)$")]
async fn then_float_result_is(world: &mut LiarWorld, expected: String) {
    if let Some(ref err) = world.error {
        panic!("PENDING: {}", err);
    }

    let result = world.result.as_ref().expect("No result");
    let actual = format_value(result);

    // Parse both as f64 for comparison to handle formatting differences
    let expected_f: f64 = expected.parse().expect("Invalid expected float");
    let actual_f: f64 = actual
        .parse()
        .unwrap_or_else(|_| panic!("Invalid actual float: {}", actual));

    assert!(
        (expected_f - actual_f).abs() < 1e-10,
        "Expected {} but got {}",
        expected,
        actual
    );
}

#[tokio::main]
async fn main() {
    if use_liarliar() {
        LiarWorld::cucumber()
            .with_runner(Basic::default().max_concurrent_scenarios(1))
            .steps(LiarWorld::collection())
            .run_and_exit("features")
            .await;
    } else {
        LiarWorld::run("features").await;
    }
}
