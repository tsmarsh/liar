//! Specification tests for liar to lIR compilation
//!
//! Verifies that liar source produces the expected lIR output.

use cucumber::{given, then, when, World};
use liar_spec::{compile_to_lir, compile_to_lir_liarliar, use_liarliar};

#[derive(Debug, Default, World)]
pub struct SpecWorld {
    /// Liar source code
    source: String,
    /// Compiled lIR output
    lir_output: Option<String>,
    /// Error message if compilation failed
    error: Option<String>,
}

impl SpecWorld {
    fn compile(&mut self) {
        let result = if use_liarliar() {
            compile_to_lir_liarliar(&self.source)
        } else {
            compile_to_lir(&self.source)
        };

        match result {
            Ok(lir) => {
                self.lir_output = Some(lir);
                self.error = None;
            }
            Err(e) => {
                self.lir_output = None;
                self.error = Some(e);
            }
        }
    }
}

// --- Given Steps ---

#[given(regex = "^the liar expression (.+)$")]
async fn given_expression(world: &mut SpecWorld, expr: String) {
    // Wrap expression in a test function for compilation
    world.source = format!("(defun test () {})", expr);
}

#[given(regex = "^the liar code \\(defun (.*)$")]
async fn given_defun(world: &mut SpecWorld, rest: String) {
    world.source.push_str("(defun ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the liar code \\(defmacro (.*)$")]
async fn given_defmacro(world: &mut SpecWorld, rest: String) {
    world.source.push_str("(defmacro ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(regex = "^the liar code \\(defstruct (.*)$")]
async fn given_defstruct(world: &mut SpecWorld, rest: String) {
    world.source.push_str("(defstruct ");
    world.source.push_str(&rest);
    world.source.push('\n');
}

#[given(expr = "the liar code:")]
async fn given_multiline_code(world: &mut SpecWorld, step: &cucumber::gherkin::Step) {
    world.source = step.docstring.clone().unwrap_or_default();
}

// --- When Steps ---

#[when("I compile to lIR")]
async fn when_compile(world: &mut SpecWorld) {
    world.compile();
}

// --- Then Steps ---

#[then("compilation succeeds")]
async fn then_compilation_succeeds(world: &mut SpecWorld) {
    if let Some(ref err) = world.error {
        panic!("Expected compilation to succeed, but got error: {}", err);
    }
    assert!(
        world.lir_output.is_some(),
        "Expected lIR output but got none"
    );
}

#[then(regex = "^the output does not contain (.+)$")]
async fn then_output_does_not_contain(world: &mut SpecWorld, pattern: String) {
    if let Some(ref err) = world.error {
        panic!("Compilation failed: {}", err);
    }
    let lir = world.lir_output.as_ref().expect("No lIR output");
    assert!(
        !lir.contains(&pattern),
        "Expected lIR to NOT contain '{}', but it does:\n{}",
        pattern,
        lir
    );
}

#[then(regex = "^the output contains (.+)$")]
async fn then_output_contains(world: &mut SpecWorld, pattern: String) {
    if let Some(ref err) = world.error {
        panic!("Compilation failed: {}", err);
    }
    let lir = world.lir_output.as_ref().expect("No lIR output");
    assert!(
        lir.contains(&pattern),
        "Expected lIR to contain '{}', but got:\n{}",
        pattern,
        lir
    );
}

#[tokio::main]
async fn main() {
    SpecWorld::run("features").await;
}
