## Summary
Generate lIR from typed, ownership-checked AST.

## Current Status
Basic codegen is implemented and working. The minimal pipeline (parse → resolve → codegen)
produces valid lIR that evaluates correctly through `lir`.

### What's Done
- Primitives: int, float, bool, string, nil → lIR literals
- Arithmetic: +, -, *, /, rem → add, sub, mul, sdiv, srem
- Comparison: =, !=, <, >, <=, >= → icmp eq/ne/slt/sgt/sle/sge
- Boolean: not, and, or → xor, and, or
- Conditionals: if → select (simple cases)
- Let bindings: let, plet → lIR let
- Do blocks: sequence expressions with discarded values
- Function definitions: defun → define with entry block
- Function calls: (f args...) → (call @f args...)
- Struct definitions: defstruct → lIR defstruct

### What's Blocked (needs other passes first)
- **Closures** → Requires closure analysis (ntdny) to identify captures
- **Pattern matching** → Requires type info for exhaustiveness checking
- **Drop insertion** → Requires ownership analysis (al4of) to know what to drop
- **Type-aware ops** → Requires type inference (ilqr5) to choose add vs fadd

## Dependencies
This story is blocked on:
1. **ilqr5** - Type inference: needed to emit correct typed operations (add vs fadd)
2. **al4of** - Ownership checking: needed to insert drop calls
3. **ntdny** - Closure analysis: needed to compile lambdas

## Translation rules (reference)

### Primitives
```lisp
42                   → (i64 42)
3.14                 → (double 3.14)
true                 → (i1 1)
false                → (i1 0)
```

### Functions
```lisp
; liar
(defun add (a b)
  (+ a b))

; lIR (current output)
(define (add i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))
```

### Closures (future - needs ntdny)
```lisp
; liar
(let ((x 10))
  (fn (y) (+ x y)))

; lIR
(defstruct closure_env_0 (i64))

(define (closure_fn_0 i64) ((ptr env) (i64 y))
  (block entry
    (let ((x (load i64 (getelementptr %struct.closure_env_0 env (i64 0) (i32 0)))))
      (ret (add x y)))))
```

### Pattern matching (future - needs type info)
```lisp
; liar
(match x
  (0 "zero")
  (1 "one")
  (_ "many"))

; lIR - compiles to nested conditionals with phi
```

### Memory management (future - needs al4of)
```lisp
; liar
(let ((x (make-big-thing)))
  (use x))
; x dropped here

; lIR
(let ((x (call @make_big_thing)))
  (call @use x)
  (call @drop_big_thing x))  ; inserted by compiler
```

## Acceptance criteria
- [x] Basic expression types compile (int, float, bool, var, call, if, let, do)
- [x] Functions compile with correct signatures
- [x] Output is valid lIR (parseable by lir-core) ✓ verified with lir evaluator
- [ ] Closures compile to struct + function (blocked on ntdny)
- [ ] Structs compile with proper alloca/GEP (blocked on type info)
- [ ] Pattern matching compiles to branches (blocked on type info)
- [ ] Drop calls inserted at scope exits (blocked on al4of)
- [ ] Type-aware arithmetic (add vs fadd) (blocked on ilqr5)
