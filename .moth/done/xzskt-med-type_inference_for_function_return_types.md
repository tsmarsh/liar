# Type Inference for Function Return Types

**ID:** xzskt
**Priority:** Medium
**Status:** Ready

## Problem

Protocol dispatch requires the compiler to know the concrete type of the receiver
to determine which implementation to call. Currently, type inference does not
track return types of function calls.

### Current limitation

This works:
```lisp
(defun test ()
  (let ((f (ImmediateFuture 42)))  ; Direct struct instantiation
    (poll f 0)))                    ; Compiler knows f is ImmediateFuture
```

This fails:
```lisp
(defun immediate (v) (ImmediateFuture v))

(defun test ()
  (let ((f (immediate 42)))  ; Function call - type unknown
    (poll f 0)))             ; Error: cannot determine type of receiver
```

Error message:
```
codegen error: cannot determine type of receiver for protocol method 'poll' - expected struct type
```

## Root Cause

The `infer.rs` module doesn't propagate return type information from function
definitions through call sites. When a variable is bound to a function call result,
the compiler loses track of what type it is.

## Solution

1. Add return type tracking to `infer.rs`:
   - When analyzing `defun`, record the inferred return type
   - When analyzing `Call`, look up the callee's return type
   - Propagate this through let bindings

2. For struct constructors (like `ImmediateFuture`):
   - These are effectively functions that return the struct type
   - Ensure constructor calls are typed as returning the struct

## Files to Modify

- `liar/src/infer.rs` - Add return type tracking
- `liar/src/codegen.rs` - Use inferred types for protocol dispatch
- Possibly `liar/src/ast.rs` - Store inferred return types

## Acceptance Criteria

- [ ] `(let ((f (immediate 42))) (poll f 0))` compiles and runs
- [ ] Protocol methods work on values returned from functions
- [ ] No regression in existing tests

## Related

- Discovered while implementing async IO (moth dhjr3)
- Affects any code that uses protocol methods on function return values
