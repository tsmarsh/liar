# Unified Macro Evaluation via JIT

## Summary

Replace the separate Rust-based macro interpreter (`eval.rs`) with JIT compilation of macro bodies, using the same execution path as the REPL. Macros should be able to call any liar function that's been defined, not just a hardcoded subset reimplemented in Rust.

## Motivation

Currently there are two separate interpreters:

1. **Runtime**: liar → lIR → LLVM JIT → native execution (used by REPL)
2. **Compile-time**: `eval.rs` — a mini-interpreter in Rust that reimplements `+`, `-`, `cons`, `first`, `rest`, `map`, `filter`, `reduce`, etc.

This duplication means:
- Every new stdlib function must be implemented twice
- Macros can't use the full stdlib (e.g., `hash-map`, `vec-map-fast`, user-defined functions)
- Bug fixes need to happen in two places
- The macro system is artificially limited

The REPL already proves that JIT compilation works incrementally. The macro system should use the same machinery.

## Current Architecture

```
Macro expansion (eval.rs):
  macro body → Rust interpreter → Value → Expr AST

REPL execution (session.rs + incremental.rs):
  expression → liar::compile() → lIR → IncrementalJit → native call → Value
```

## Proposed Architecture

```
Macro expansion:
  macro body → liar::compile() → lIR → IncrementalJit → native call → Value → Expr AST
```

The macro evaluator becomes a thin wrapper around the same JIT used by the REPL.

## Implementation Plan

### 1. Create a Compile-Time JIT Context

New module `liar/src/macro_jit.rs`:

```rust
use inkwell::context::Context;
use lir_codegen::IncrementalJit;

/// JIT context for macro evaluation
/// Lazily initialized, lives for the duration of compilation
pub struct MacroJit<'ctx> {
    context: &'ctx Context,
    jit: IncrementalJit<'ctx>,
    /// Functions already compiled for macro use
    available_functions: HashSet<String>,
}

impl<'ctx> MacroJit<'ctx> {
    pub fn new(context: &'ctx Context) -> Result<Self, String> {
        let jit = IncrementalJit::new(context)?;
        Ok(Self {
            context,
            jit,
            available_functions: HashSet::new(),
        })
    }
    
    /// Ensure a function is available for macro evaluation
    pub fn ensure_function(&mut self, name: &str, source: &str) -> Result<(), Error> {
        if self.available_functions.contains(name) {
            return Ok(());
        }
        // Compile and add to JIT
        // ...
    }
    
    /// Evaluate an expression, return a Value
    pub fn eval(&mut self, expr: &str) -> Result<Value, Error> {
        // Wrap in thunk, compile, execute
        // Similar to Session::eval_expression
    }
}
```

### 2. Bootstrap the Stdlib for Macros

At the start of compilation, compile core functions into the macro JIT:

```rust
impl MacroJit {
    pub fn bootstrap_stdlib(&mut self) -> Result<(), Error> {
        // Compile essential functions for macro use
        let stdlib = include_str!("../../lib/stdlib.liar");
        let seq = include_str!("../../lib/seq.liar");
        
        // Parse and compile each definition
        // These become available for macros to call
    }
}
```

### 3. Value Marshalling

The tricky part: converting between JIT memory and Rust `Value` types.

```rust
/// Convert a JIT result to a macro Value
fn jit_to_value(result: lir_codegen::Value) -> macro_eval::Value {
    match result {
        lir_codegen::Value::I64(n) => macro_eval::Value::Int(n),
        lir_codegen::Value::Ptr(0) => macro_eval::Value::Nil,
        lir_codegen::Value::Ptr(p) => {
            // Read the Cons/List structure from memory
            // This requires knowing the memory layout
            unsafe { read_list_from_ptr(p) }
        }
        // ...
    }
}

/// Convert a macro Value to JIT-compatible form
fn value_to_jit(value: &macro_eval::Value) -> Result<String, Error> {
    // Generate liar source that constructs this value
    match value {
        Value::Int(n) => Ok(format!("{}", n)),
        Value::List(items) => {
            let inner = items.iter()
                .map(value_to_jit)
                .collect::<Result<Vec<_>, _>>()?
                .join(" ");
            Ok(format!("(list {})", inner))
        }
        Value::Symbol(s) => Ok(format!("'{}", s)),
        // ...
    }
}
```

### 4. Modify Evaluator to Use JIT

Update `eval.rs` to delegate to JIT for function calls:

```rust
impl Evaluator {
    fn eval_builtin(
        &self,
        env: &Env,
        name: &str,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Option<Value>> {
        // Keep a small set of primitives in Rust for bootstrapping:
        // - quote, quasiquote, unquote (AST manipulation)
        // - if, let (control flow during macro expansion)
        // - gensym (needs global counter)
        
        match name {
            "quote" | "quasiquote" | "unquote" | "if" | "let" | "gensym" => {
                // Handle in Rust (current implementation)
            }
            _ => {
                // Delegate to JIT
                self.eval_via_jit(name, args, span)
            }
        }
    }
    
    fn eval_via_jit(
        &self,
        name: &str,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Option<Value>> {
        // 1. Convert args to liar source
        // 2. Build call expression: (name arg1 arg2 ...)
        // 3. Compile and execute via MacroJit
        // 4. Convert result back to Value
    }
}
```

### 5. Phasing: Which Functions Are Available?

A function is available to macros if it's defined **before** the macro that uses it.

```lisp
;; This works - helper defined before macro
(defun double (x) (* x 2))

(defmacro make-doubled (n)
  (double n))  ; calls double at compile time

;; This fails - macro uses function defined later
(defmacro bad-macro (n)
  (later-fn n))  ; ERROR: later-fn not yet available

(defun later-fn (x) x)
```

The compiler processes top-level forms in order:
1. See `defun` → compile into macro JIT
2. See `defmacro` → register macro (can now use previously compiled functions)
3. See macro call → expand using JIT

### 6. Gradual Migration

Phase 1: Add JIT infrastructure, keep Rust builtins as fallback
```rust
fn eval_builtin(...) {
    // Try JIT first
    if let Ok(result) = self.eval_via_jit(name, args, span) {
        return Ok(Some(result));
    }
    // Fall back to Rust implementation
    match name {
        "+" => { /* current Rust impl */ }
        // ...
    }
}
```

Phase 2: Remove Rust implementations one by one as JIT proves stable

Phase 3: Only keep AST-manipulation primitives in Rust

## Memory Layout Contract

For marshalling to work, the compiler and macro system must agree on memory layout:

```rust
// Cons cell layout (must match defstruct Cons)
#[repr(C)]
struct ConsCell {
    head: i64,
    tail: *const ConsCell,  // null for nil
}

// Reading a list from JIT memory
unsafe fn read_list_from_ptr(ptr: u64) -> Value {
    if ptr == 0 {
        return Value::Nil;
    }
    let cell = &*(ptr as *const ConsCell);
    let head = Value::Int(cell.head);
    let tail = read_list_from_ptr(cell.tail as u64);
    Value::List(vec![head].into_iter().chain(as_list(tail)).collect())
}
```

## Acceptance Criteria

- [ ] MacroJit struct that wraps IncrementalJit for compile-time evaluation
- [ ] Stdlib functions compiled into macro JIT at start of compilation
- [ ] Macros can call any previously-defined function
- [ ] Value marshalling works for: integers, booleans, nil, lists, symbols
- [ ] Existing macro tests continue to pass
- [ ] New test: macro calling a user-defined function
- [ ] New test: macro calling stdlib `map`/`filter`/`reduce`
- [ ] Performance: macro expansion not significantly slower

## Test Scenarios

```gherkin
Feature: Macros use JIT-compiled functions

  Scenario: Macro calls user-defined function
    Given the program:
      """
      (defun double (x) (* x 2))
      
      (defmacro make-constant (n)
        (double n))
      
      (defun main ()
        (make-constant 21))
      """
    When I compile and run it
    Then the result should be 42

  Scenario: Macro calls stdlib map
    Given the program:
      """
      (defmacro sum-doubled (items)
        (reduce + 0 (map (fn (x) (* x 2)) items)))
      
      (defun main ()
        (sum-doubled (list 1 2 3)))
      """
    When I compile and run it
    Then the result should be 12

  Scenario: Macro cannot call later-defined function
    Given the program:
      """
      (defmacro bad (n)
        (later n))
      
      (defun later (x) x)
      
      (defun main () (bad 1))
      """
    When I compile it
    Then compilation should fail with "later not defined"

  Scenario: Quasiquote still works with JIT
    Given the program:
      """
      (defun make-op (op a b)
        `(,op ,a ,b))
      
      (defmacro math (op a b)
        (make-op op a b))
      
      (defun main ()
        (math + 10 20))
      """
    When I compile and run it
    Then the result should be 30
```

## Risks and Mitigations

**Risk**: Memory layout mismatch between Rust and JIT
**Mitigation**: Use `#[repr(C)]` structs, add runtime assertions, test marshalling extensively

**Risk**: Macro expansion becomes slow (JIT overhead)
**Mitigation**: Cache compiled functions, only recompile when source changes

**Risk**: Circular dependency (macro needs function that needs macro)
**Mitigation**: Process in source order, error on forward references

**Risk**: Debugging macro expansion becomes harder
**Mitigation**: Add `--debug-macros` flag that prints JIT IR and execution trace

## Future Work

- **Macro hygiene**: Gensym is still in Rust, but could move to JIT with a global atom
- **Reader macros**: Once we have `read`, macros could transform source before parsing
- **Compile-time I/O**: Macros could read files, enabling `include` functionality

## References

- `liar/src/eval.rs` — current Rust macro interpreter (to be replaced)
- `liar-repl/src/session.rs` — REPL JIT usage pattern
- `lir-codegen/src/incremental.rs` — IncrementalJit implementation
- `lib/stdlib.liar`, `lib/seq.liar` — functions macros should be able to call
