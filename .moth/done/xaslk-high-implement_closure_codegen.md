# Implement Closure Codegen

**ID:** xaslk
**Priority:** High
**Status:** Ready

## Problem

Higher-order functions like `comp`, `flip`, `constantly` require closures to capture
variables from their enclosing scope. Currently, lambdas that capture variables fail
with "lambdas require closure analysis (not yet implemented)".

## Current State

The liar compiler already has:
- `closures.rs` - Closure analysis (capture detection, closure coloring)
- Lambda parsing in `parser.rs`
- Lambda AST nodes in `ast.rs`

What's missing:
- Codegen for lambdas that captures variables
- Runtime representation of closures (function pointer + environment)

## Implementation Plan

1. **Closure representation in lIR:**
   - A closure is a struct: `{ fn_ptr: ptr, env: ptr }`
   - The environment is a struct containing captured variables
   - Function takes environment as first hidden parameter

2. **Codegen changes:**
   - When generating a lambda with captures:
     a. Generate the function with extra `env` parameter
     b. Create environment struct with captured values
     c. Return closure struct with fn_ptr and env

3. **Call site changes:**
   - When calling through a closure:
     a. Extract fn_ptr and env from closure struct
     b. Call fn_ptr with env as first argument

## Example

```lisp
(defun constantly (v)
  (fn (x) v))  ; captures v
```

Should generate:
```
; Environment struct for the lambda
(defstruct constantly_env (v: i64))

; The actual lambda function
(define (constantly_lambda i64) ((ptr env) (i64 x))
  (block entry
    (ret (extractvalue (load constantly_env env) 0))))

; The constantly function
(define (constantly ptr) ((i64 v))
  (block entry
    (let ((env (alloca constantly_env)))
      (store (insertvalue (constantly_env undef) v 0) env)
      (ret { (ptr @constantly_lambda) env }))))
```

## Files to Modify

- `liar/src/codegen.rs` - Add closure codegen
- `lir-core/src/ast.rs` - May need closure struct type
- `liar/src/closures.rs` - Already has analysis, verify it's complete

## Acceptance Criteria

- [ ] `(defun constantly (v) (fn (x) v))` compiles
- [ ] `((constantly 5) 100)` returns 5
- [ ] `comp`, `flip` work correctly
- [ ] Existing tests pass

## Dependencies

- Closure analysis already implemented
- Structs already work
