## Summary
Add ownership pointer types to lIR: own, ref, refmut.

## Types
```lisp
own T       ; Owned pointer — dropped when out of scope
ref T       ; Shared borrow — read-only, lifetime-bound
refmut T    ; Mutable borrow — exclusive, lifetime-bound
```

## AST Additions
```rust
pub enum ParamType {
    Scalar(ScalarType),
    Ptr,
    Own(Box<ScalarType>),
    Ref(Box<ScalarType>),
    RefMut(Box<ScalarType>),
    Rc(Box<ScalarType>),
}
```

## Operations
```lisp
(alloc own T)           ; Allocate owned
(borrow ref x)          ; Create shared borrow
(borrow refmut x)       ; Create mutable borrow
(drop x)                ; Explicit drop
(move x)                ; Explicit move
```

## Codegen
Ownership types compile to raw pointers at LLVM level.
The safety is enforced by the verifier, not runtime.

```rust
// own T -> ptr in LLVM
// ref T -> ptr in LLVM
// refmut T -> ptr in LLVM

// alloc own T -> alloca or malloc depending on escape analysis
// drop x -> call destructor, then free if heap-allocated
// borrow ref x -> just the pointer (no-op at runtime)
// borrow refmut x -> just the pointer (no-op at runtime)
```

## Test Cases (Parser/Codegen Only)
```gherkin
Scenario: Parse own type
  Given the expression (define (take void) ((own i64 x)) (block entry (drop x) (ret)))
  Then parsing succeeds

Scenario: Parse ref type
  Given the expression (define (peek i64) ((ref i64 x)) (block entry (ret (load i64 x))))
  Then parsing succeeds

Scenario: Parse refmut type
  Given the expression (define (poke void) ((refmut i64 x)) (block entry (store (i64 42) x) (ret)))
  Then parsing succeeds

Scenario: Alloc and drop
  Given the expression (define (test i64) () (block entry (let ((x (alloc own i64))) (store (i64 42) x) (let ((v (load i64 x))) (drop x) (ret v)))))
  When I call test
  Then the result is (i64 42)
```

## Acceptance Criteria
- [ ] own, ref, refmut types parse
- [ ] alloc own T works
- [ ] borrow ref/refmut works
- [ ] drop works
- [ ] Compiles to correct LLVM IR
- [ ] Verification is separate moth
