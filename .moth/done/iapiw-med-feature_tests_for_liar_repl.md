## Summary

Create BDD test infrastructure for liar: REPL and cucumber feature tests to drive stdlib development.

**Depends on:** r3f4c (codegen emits lIR AST)

## Goal

Same test-driven flow lIR has (271 passing scenarios), but for liar:

```gherkin
Feature: Standard Library - Arithmetic

  Scenario: inc increments by 1
    When I evaluate (inc 5)
    Then the result is 6
```

## Architecture

```
Feature file → Cucumber step → liar compiler → lIR AST → JIT → result
```

No string serialization in the hot path.

## Components

### 1. liar-repl Crate

**Location:** `liar-repl/`

**Cargo.toml:**
```toml
[package]
name = "liar-repl"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "liar-repl"
path = "src/main.rs"

[dependencies]
liar = { path = "../liar" }
lir-core = { path = "../lir-core" }
lir-codegen = { path = "../lir-codegen" }
inkwell = { version = "0.4", features = ["llvm17-0"] }
rustyline = "13"
```

**src/main.rs:**
```rust
//! liar Interactive REPL
//!
//! Compiles liar to lIR AST, then JIT executes.

use inkwell::context::Context;
use lir_codegen::JitEngine;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const BANNER: &str = r#"
liar REPL - A Lisp that doesn't lie about memory
Type expressions to evaluate, :help for commands, :quit to exit
"#;

fn print_help() {
    println!(r#"
Commands:
  :help, :h     Show this help
  :quit, :q     Exit the REPL
  :load FILE    Load and evaluate a file
  :type EXPR    Show inferred type without evaluating

Examples:
  (+ 1 2)                ; => 3
  (defun square (x) (* x x))
  (square 5)             ; => 25
  (let ((x 10)) (* x x)) ; => 100
"#);
}

struct ReplState {
    context: Context,
    /// Accumulated definitions (functions, structs, etc.)
    definitions: String,
}

impl ReplState {
    fn new() -> Self {
        Self {
            context: Context::create(),
            definitions: String::new(),
        }
    }

    fn eval(&mut self, input: &str) -> Result<String, String> {
        // Combine accumulated definitions with new input
        let full_source = format!("{}\n{}", self.definitions, input);
        
        // Compile liar → lIR AST
        let lir_module = liar::compile(&full_source)
            .map_err(|errs| errs.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"))?;
        
        // If this is a definition, accumulate it
        if input.trim().starts_with("(def") {
            self.definitions.push_str(input);
            self.definitions.push('\n');
            return Ok("defined".to_string());
        }
        
        // JIT execute
        let jit = JitEngine::new(&self.context);
        let result = jit.eval_module(&lir_module)
            .map_err(|e| format!("Evaluation error: {}", e))?;
        
        Ok(format_result(&result))
    }
}

fn format_result(value: &lir_codegen::Value) -> String {
    use lir_codegen::Value;
    match value {
        Value::I1(b) => if *b { "true" } else { "false" }.to_string(),
        Value::I8(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I64(n) => n.to_string(),
        Value::Float(f) => format!("{}", f),
        Value::Double(d) => format!("{}", d),
        Value::Ptr(0) => "nil".to_string(),
        Value::Ptr(p) => format!("<ptr {:x}>", p),
        // TODO: collections, keywords, etc.
        _ => format!("{:?}", value),
    }
}

fn eval_line(state: &mut ReplState, line: &str) {
    let line = line.trim();
    if line.is_empty() {
        return;
    }

    // Handle commands
    if line.starts_with(':') {
        match line.split_whitespace().next().unwrap() {
            ":help" | ":h" => print_help(),
            ":quit" | ":q" => std::process::exit(0),
            ":load" => {
                let path = line.strip_prefix(":load").unwrap().trim();
                match std::fs::read_to_string(path) {
                    Ok(contents) => {
                        match state.eval(&contents) {
                            Ok(result) => println!("{}", result),
                            Err(e) => eprintln!("{}", e),
                        }
                    }
                    Err(e) => eprintln!("Error loading file: {}", e),
                }
            }
            _ => println!("Unknown command: {}. Type :help for help.", line),
        }
        return;
    }

    match state.eval(line) {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("{}", e),
    }
}

fn main() {
    println!("{}", BANNER);

    let mut rl = match DefaultEditor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize editor: {}", e);
            std::process::exit(1);
        }
    };

    let mut state = ReplState::new();

    loop {
        let readline = rl.readline("liar> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                eval_line(&mut state, &line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
```

### 2. liar-cert Crate (Cucumber Tests)

**Location:** `liar-cert/`

**Cargo.toml:**
```toml
[package]
name = "liar-cert"
version = "0.1.0"
edition = "2021"

[[test]]
name = "cert"
harness = false

[dependencies]
liar = { path = "../liar" }
lir-core = { path = "../lir-core" }
lir-codegen = { path = "../lir-codegen" }
inkwell = { version = "0.4", features = ["llvm17-0"] }
cucumber = "0.20"
tokio = { version = "1", features = ["macros", "rt-multi-thread"] }
```

**tests/cert.rs:**
```rust
//! Certification tests for liar
//!
//! Uses cucumber features to verify liar semantics.
//!
//! Test semantics (same as lIR):
//! - Exit code != 0: Pending (yellow) — not implemented yet
//! - Exit code == 0, output mismatch: Failed (red) — implemented wrong
//! - Exit code == 0, output matches: Passed (green) — works

use cucumber::{given, then, when, World};
use inkwell::context::Context;
use lir_codegen::{JitEngine, Value};

#[derive(Debug, Default, World)]
pub struct LiarWorld {
    /// Accumulated source (stdlib + definitions)
    source: String,
    /// Expression to evaluate
    expression: String,
    /// Result of evaluation
    result: Option<Value>,
    /// Error message if compilation/evaluation failed
    error: Option<String>,
}

impl LiarWorld {
    fn eval(&mut self, expr: &str) {
        let full_source = format!("{}\n{}", self.source, expr);
        
        // Compile liar → lIR AST
        let lir_module = match liar::compile(&full_source) {
            Ok(m) => m,
            Err(errs) => {
                self.error = Some(errs.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"));
                return;
            }
        };
        
        // JIT execute
        let context = Context::create();
        let jit = JitEngine::new(&context);
        match jit.eval_module(&lir_module) {
            Ok(val) => self.result = Some(val),
            Err(e) => self.error = Some(format!("Evaluation error: {}", e)),
        }
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::I1(b) => if *b { "true" } else { "false" }.to_string(),
        Value::I8(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I64(n) => n.to_string(),
        Value::Float(f) => format!("{}", f),
        Value::Double(d) => format!("{}", d),
        Value::Ptr(0) => "nil".to_string(),
        Value::Ptr(p) => format!("<ptr {:x}>", p),
        _ => format!("{:?}", value),
    }
}

// --- Step Definitions ---

#[given("the standard library is loaded")]
async fn given_stdlib(world: &mut LiarWorld) {
    // Load stdlib.liar
    let stdlib = include_str!("../../lib/stdlib.liar");
    world.source = stdlib.to_string();
}

#[given(regex = r"^the definition (.+)$")]
async fn given_definition(world: &mut LiarWorld, def: String) {
    world.source.push_str(&def);
    world.source.push('\n');
}

#[when(regex = r"^I evaluate (.+)$")]
async fn when_evaluate(world: &mut LiarWorld, expr: String) {
    world.expression = expr.clone();
    world.eval(&expr);
}

#[then(regex = r"^the result is (.+)$")]
async fn then_result_is(world: &mut LiarWorld, expected: String) {
    if let Some(ref err) = world.error {
        panic!("PENDING: {}", err);
    }
    
    let result = world.result.as_ref().expect("No result");
    let actual = format_value(result);
    
    assert_eq!(actual, expected, "Expected {} but got {}", expected, actual);
}

#[then(regex = r#"^it errors with "(.+)"$"#)]
async fn then_errors_with(world: &mut LiarWorld, expected_msg: String) {
    match &world.error {
        Some(err) => {
            assert!(err.contains(&expected_msg), 
                "Expected error containing '{}', got '{}'", expected_msg, err);
        }
        None => {
            panic!("Expected error but evaluation succeeded with {:?}", world.result);
        }
    }
}

#[then("evaluation succeeds")]
async fn then_evaluation_succeeds(world: &mut LiarWorld) {
    if let Some(ref err) = world.error {
        panic!("PENDING: {}", err);
    }
}

#[tokio::main]
async fn main() {
    LiarWorld::run("features").await;
}
```

### 3. Feature Files

**Location:** `liar-cert/features/`

**features/arithmetic.feature:**
```gherkin
Feature: Arithmetic Operations

  Background:
    Given the standard library is loaded

  Scenario: inc increments by 1
    When I evaluate (inc 5)
    Then the result is 6

  Scenario: dec decrements by 1
    When I evaluate (dec 5)
    Then the result is 4

  Scenario: abs of positive number
    When I evaluate (abs 5)
    Then the result is 5

  Scenario: abs of negative number
    When I evaluate (abs -5)
    Then the result is 5

  Scenario: square
    When I evaluate (square 4)
    Then the result is 16

  Scenario: max returns larger
    When I evaluate (max 3 7)
    Then the result is 7

  Scenario: min returns smaller
    When I evaluate (min 3 7)
    Then the result is 3

  Scenario: clamp within range
    When I evaluate (clamp 5 0 10)
    Then the result is 5

  Scenario: clamp below range
    When I evaluate (clamp -5 0 10)
    Then the result is 0

  Scenario: clamp above range
    When I evaluate (clamp 15 0 10)
    Then the result is 10
```

**features/predicates.feature:**
```gherkin
Feature: Numeric Predicates

  Background:
    Given the standard library is loaded

  Scenario: even? on even number
    When I evaluate (even? 4)
    Then the result is true

  Scenario: even? on odd number
    When I evaluate (even? 5)
    Then the result is false

  Scenario: odd? on odd number
    When I evaluate (odd? 5)
    Then the result is true

  Scenario: odd? on even number
    When I evaluate (odd? 4)
    Then the result is false

  Scenario: zero? on zero
    When I evaluate (zero? 0)
    Then the result is true

  Scenario: zero? on non-zero
    When I evaluate (zero? 5)
    Then the result is false

  Scenario: pos? on positive
    When I evaluate (pos? 5)
    Then the result is true

  Scenario: pos? on zero
    When I evaluate (pos? 0)
    Then the result is false

  Scenario: neg? on negative
    When I evaluate (neg? -5)
    Then the result is true
```

**features/math.feature:**
```gherkin
Feature: Math Functions

  Background:
    Given the standard library is loaded

  Scenario: factorial of 0
    When I evaluate (factorial 0)
    Then the result is 1

  Scenario: factorial of 1
    When I evaluate (factorial 1)
    Then the result is 1

  Scenario: factorial of 5
    When I evaluate (factorial 5)
    Then the result is 120

  Scenario: factorial-tail of 5
    When I evaluate (factorial-tail 5)
    Then the result is 120

  Scenario: gcd of 12 and 8
    When I evaluate (gcd 12 8)
    Then the result is 4

  Scenario: gcd of 17 and 13 (coprime)
    When I evaluate (gcd 17 13)
    Then the result is 1

  Scenario: lcm of 4 and 6
    When I evaluate (lcm 4 6)
    Then the result is 12

  Scenario: pow base case
    When I evaluate (pow 2 0)
    Then the result is 1

  Scenario: pow of 2^10
    When I evaluate (pow 2 10)
    Then the result is 1024

  Scenario: fib of 0
    When I evaluate (fib-fast 0)
    Then the result is 0

  Scenario: fib of 1
    When I evaluate (fib-fast 1)
    Then the result is 1

  Scenario: fib of 10
    When I evaluate (fib-fast 10)
    Then the result is 55

  Scenario: sum-to 10
    When I evaluate (sum-to 10)
    Then the result is 55
```

**features/functions.feature:**
```gherkin
Feature: Function Composition

  Background:
    Given the standard library is loaded

  Scenario: identity returns its argument
    When I evaluate (identity 42)
    Then the result is 42

  Scenario: constantly returns same value
    When I evaluate ((constantly 5) 100)
    Then the result is 5

  Scenario: comp composes two functions
    Given the definition (defun double (x) (* x 2))
    When I evaluate ((comp inc double) 3)
    Then the result is 7

  Scenario: partial application
    When I evaluate ((partial + 10) 5)
    Then the result is 15

  Scenario: flip reverses argument order
    When I evaluate ((flip -) 3 10)
    Then the result is 7

  Scenario: complement negates predicate
    When I evaluate ((complement even?) 5)
    Then the result is true
```

**features/nil.feature:**
```gherkin
Feature: Nil-safe Operations

  Background:
    Given the standard library is loaded

  Scenario: nil? on nil
    When I evaluate (nil? nil)
    Then the result is true

  Scenario: nil? on value
    When I evaluate (nil? 5)
    Then the result is false

  Scenario: some? on value
    When I evaluate (some? 5)
    Then the result is true

  Scenario: some? on nil
    When I evaluate (some? nil)
    Then the result is false

  Scenario: or-else with value
    When I evaluate (or-else 5 10)
    Then the result is 5

  Scenario: or-else with nil
    When I evaluate (or-else nil 10)
    Then the result is 10
```

## Directory Structure

```
liar-main/
├── lib/
│   └── stdlib.liar          # Standard library (already exists)
├── liar-repl/
│   ├── Cargo.toml
│   └── src/
│       └── main.rs
├── liar-cert/
│   ├── Cargo.toml
│   ├── features/
│   │   ├── arithmetic.feature
│   │   ├── predicates.feature
│   │   ├── math.feature
│   │   ├── functions.feature
│   │   └── nil.feature
│   └── tests/
│       └── cert.rs
└── Cargo.toml               # Add liar-repl and liar-cert to workspace
```

## Workspace Cargo.toml Update

Add to root `Cargo.toml`:
```toml
[workspace]
members = [
    # ... existing members ...
    "liar-repl",
    "liar-cert",
]
```

## Acceptance Criteria

- [ ] `cargo build -p liar-repl` succeeds
- [ ] `cargo run -p liar-repl` starts REPL
- [ ] REPL evaluates `(+ 1 2)` → `3`
- [ ] REPL `:load lib/stdlib.liar` works
- [ ] REPL accumulates definitions across inputs
- [ ] `cargo test -p liar-cert --test cert` runs features
- [ ] Feature files exist for stdlib arithmetic, predicates, math, functions, nil
- [ ] Tests show proper pending/pass/fail status

## Notes for Agent

- This depends on r3f4c (codegen refactor) being complete first
- If JitEngine doesn't have `eval_module`, may need to add it or use existing eval path
- The `include_str!` for stdlib assumes relative path - adjust if needed
- Start with arithmetic.feature to verify the pipeline works end-to-end
- Format results as liar values (true/false not 1/0, nil not ptr 0)
