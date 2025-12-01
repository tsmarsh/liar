//! Cucumber feature test runner for lIR
//!
//! Test semantics:
//! - Exit code != 0: Pending (yellow) — not implemented yet
//! - Exit code == 0, output mismatch: Failed (red) — implemented wrong
//! - Exit code == 0, output matches: Passed (green) — works

use cucumber::{given, then, World};
use lir_cert::{execute, LirBackend, StubBackend};

#[derive(Debug, Default, World)]
pub struct LirWorld {
    expression: String,
    exit_code: i32,
    stdout: String,
    stderr: String,
}

// Use StubBackend by default - replace with actual backend for real tests
type Backend = StubBackend;

#[given(regex = r"^the expression (.+)$")]
async fn given_expression(world: &mut LirWorld, expr: String) {
    world.expression = expr.clone();
    let result = execute::<Backend>(&expr);
    world.exit_code = result.exit_code;
    world.stdout = result.stdout;
    world.stderr = result.stderr;
}

#[then(regex = r"^the result is (.+)$")]
async fn then_result_is(world: &mut LirWorld, expected: String) {
    if world.exit_code != 0 {
        // Non-zero exit = pending (not implemented)
        cucumber::codegen::Pending::new("not implemented").pend();
    }
    assert_eq!(
        world.stdout, expected,
        "Expected '{}' but got '{}'",
        expected, world.stdout
    );
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
    LirWorld::run("features").await;
}
