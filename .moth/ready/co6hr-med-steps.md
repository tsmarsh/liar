# Test harness for feature specs

The cucumber test harness needs to be generic. The crate under test provides a hook that returns the shell command for evaluating a lIR expression.

## Design

Trait in the test harness:
```rust
pub trait LirBackend {
    fn eval_command(expr: &str) -> String;
}
```

Implementations might look like:
```rust
// Interpreter
fn eval_command(expr: &str) -> String {
    format!("echo '{}' | lir-interp", expr)
}

// AOT compiler
fn eval_command(expr: &str) -> String {
    format!("echo '{}' | lir-aot", expr)
}

// JIT
fn eval_command(expr: &str) -> String {
    format!("echo '{}' | lir-jit", expr)
}
```

## Test semantics

| Exit code | Output matches | Result |
|-----------|----------------|--------|
| != 0      | —              | Pending (yellow) — not implemented yet |
| 0         | no             | Failed (red) — implemented wrong |
| 0         | yes            | Passed (green) — works |

## Step definitions
```rust
#[when(regex = r#"^I evaluate "(.+)"$"#)]
async fn evaluate(w: &mut World, expr: String) {
    let cmd = Backend::eval_command(&expr);
    let output = Command::new("sh")
        .arg("-c")
        .arg(&cmd)
        .output()
        .expect("failed to execute");
    
    w.exit_code = output.status.code().unwrap_or(-1);
    w.output = String::from_utf8_lossy(&output.stdout).trim().to_string();
    w.stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
}

#[then(regex = r#"^the result should be "(.+)"$"#)]
async fn result_is(w: &mut World, expected: String) {
    if w.exit_code != 0 {
        pending!("not implemented: {}", w.stderr);
    }
    assert_eq!(w.output, expected);
}

#[then(regex = r#"^the result should be error "(.+)"$"#)]
async fn result_is_error(w: &mut World, expected_msg: String) {
    if w.exit_code == 0 {
        panic!("expected error but got success: {}", w.output);
    }
    assert!(w.stderr.contains(&expected_msg), 
        "expected error containing '{}', got '{}'", expected_msg, w.stderr);
}
```

## Backend contract

A conforming backend must:

1. Read lIR expression from stdin
2. Exit 0 and print result to stdout if evaluation succeeds
3. Exit non-zero and print error to stderr if:
   - Opcode not implemented
   - Parse error
   - Type error
   - Runtime error (div by zero, etc.)

## Tasks

- [ ] Create `lir-test` crate with harness
- [ ] Define `LirBackend` trait
- [ ] Implement step definitions
- [ ] Wire up cucumber runner
- [ ] Test against stub backend that returns non-zero for everything (all pending)
