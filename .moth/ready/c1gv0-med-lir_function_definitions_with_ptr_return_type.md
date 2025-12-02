## Summary
Function definitions use `ScalarType` for return type, but functions often
need to return pointers (e.g., `alloca`, factory functions).

## Current limitation
```rust
pub struct FunctionDef {
    pub name: String,
    pub return_type: ScalarType,  // Can't return ptr
    pub params: Vec<Param>,
    pub blocks: Vec<BasicBlock>,
}
```

## Example that should work
```lisp
(define (make_closure ptr) ((i64 x))
  (block entry
    (let ((env (call @malloc (i64 8))))
      (ret env))))
```

## Proposed fix
Use `ReturnType` (already exists for extern declarations):

```rust
pub struct FunctionDef {
    pub name: String,
    pub return_type: ReturnType,  // Scalar or Ptr
    pub params: Vec<Param>,
    pub blocks: Vec<BasicBlock>,
}
```

## Acceptance Criteria
- [ ] Parse `(define (name ptr) ...)` for ptr return
- [ ] Generate LLVM function with ptr return type
- [ ] memory.feature alloca tests pass (they return ptr)
