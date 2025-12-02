## Summary
Function parameters use `ScalarType` but need to support `ptr` for passing
pointers to functions.

## Current limitation
```rust
pub struct Param {
    pub ty: ScalarType,  // Can't be ptr
    pub name: String,
}
```

## Example that should work
```lisp
(define (read-ptr i32) ((ptr p))
  (block entry
    (ret (load i32 p))))
```

## Proposed fix
Use `ParamType`:

```rust
pub struct Param {
    pub ty: ParamType,  // Scalar or Ptr
    pub name: String,
}
```

## Acceptance Criteria
- [ ] Parse `((ptr name) ...)` in function parameters
- [ ] Generate LLVM function with ptr parameters
- [ ] memory.feature "read-ptr" test passes
