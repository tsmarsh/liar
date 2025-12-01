//! lIR Test Harness
//!
//! Generic cucumber test harness for lIR feature specifications.
//! Backends implement the `LirBackend` trait to provide evaluation.

use std::process::Command;

/// Trait for lIR backends to implement evaluation
pub trait LirBackend {
    /// Returns the shell command to evaluate a lIR expression
    fn eval_command(expr: &str) -> String;
}

/// Result of evaluating a lIR expression
#[derive(Debug, Clone)]
pub struct EvalResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

/// Execute a lIR expression using the given backend
pub fn execute<B: LirBackend>(expr: &str) -> EvalResult {
    let cmd = B::eval_command(expr);
    let output = Command::new("sh")
        .arg("-c")
        .arg(&cmd)
        .output()
        .expect("failed to execute backend command");

    EvalResult {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).trim().to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).trim().to_string(),
    }
}

/// Stub backend that always returns non-zero (all tests pending)
pub struct StubBackend;

impl LirBackend for StubBackend {
    fn eval_command(_expr: &str) -> String {
        "exit 1".to_string()
    }
}
