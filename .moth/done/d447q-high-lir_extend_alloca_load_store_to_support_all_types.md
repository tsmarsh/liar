## Summary
Currently `Alloca`, `Load`, and `Store` only support `ScalarType`. They should
support `ptr` and potentially vectors/structs for full LLVM IR coverage.

## Current limitation
```rust
Alloca {
    ty: ScalarType,  // Can't alloca ptr or vector
    count: Option<Box<Expr>>,
}
Load {
    ty: ScalarType,  // Can't load ptr
    ptr: Box<Expr>,
}
```

## Proposed fix
```rust
// Option 1: Use ParamType (already exists, supports Scalar and Ptr)
Alloca {
    ty: ParamType,
    count: Option<Box<Expr>>,
}
Load {
    ty: ParamType,
    ptr: Box<Expr>,
}

// Option 2: Create a more comprehensive MemoryType
enum MemoryType {
    Scalar(ScalarType),
    Ptr,
    Vector(VectorType),
    Struct(String),  // Named struct reference
}
```

## Examples that should work
```lisp
; Alloca a pointer
(alloca ptr)

; Load a pointer from memory
(load ptr %ptr_to_ptr)

; Store a pointer
(store (ptr null) %dest)

; Alloca a vector (nice to have)
(alloca <4 x i32>)
```

## Acceptance Criteria
- [ ] `alloca ptr` works
- [ ] `load ptr %p` works  
- [ ] `store (ptr x) %p` works
- [ ] Update parser to accept ptr type in these positions
- [ ] Update codegen to generate correct LLVM types

## Notes
- ParamType already exists and may be sufficient
- Vectors and structs can be a follow-up if needed
