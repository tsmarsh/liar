## Summary
Implement explicit overflow handling modes: boxed (auto-promote),
wrapping (C-style), and checked (default).

## Syntax
```lisp
; Default: checked (panic on overflow)
(+ x y)

; Boxed: promotes to biginteger, never overflows
(boxed (* BIG BIGGER))

; Wrapping: C-style silent wrap
(wrapping (* x y))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Boxed(Box<Spanned<Expr>>),
    Wrapping(Box<Spanned<Expr>>),
}
```

## Codegen

### Checked (default)
```lisp
; Uses LLVM's overflow intrinsics
(let ((result (call.overflow @llvm.sadd.with.overflow.i64 a b)))
  (br (extractvalue result 1) overflow_handler continue))
```

### Wrapping
```lisp
; Just regular add, wraps naturally
(add a b)
```

### Boxed
```lisp
; Check for potential overflow, promote if needed
; Requires bigint library integration
```

## Implementation Notes
- Boxed requires bigint library (num-bigint, rug, or custom)
- Checked needs runtime panic handler
- Wrapping is simplest (current behavior)

## Acceptance Criteria
- [ ] boxed syntax parses
- [ ] wrapping syntax parses  
- [ ] Checked arithmetic panics on overflow
- [ ] Wrapping arithmetic wraps silently
- [ ] Boxed promotes to bigint
