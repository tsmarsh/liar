## Summary
Implement share (reference counting) and clone (deep copy) operations.

## Syntax
```lisp
(share x)    ; Create reference-counted value
(clone x)    ; Deep copy, returns owned value
```

## Semantics

### share
```lisp
(let ((x (share (cons 1 2))))   ; refcount=1
  (let ((y x))                   ; refcount=2 (implicit clone of rc)
    (car y))                     ; access through y
  (car x))                       ; still valid, refcount=1
                                 ; refcount=0 here, freed
```

### clone
```lisp
(let ((x (cons 1 2)))
  (let ((y (clone x)))  ; y owns a deep copy
    (set-car! y 99)     ; mutate copy
    (cons (car x)       ; x unchanged => 1
          (car y))))    ; y changed => 99
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Share(Box<Spanned<Expr>>),
    Clone(Box<Spanned<Expr>>),
}
```

## Type System
```rust
// share: T -> Rc<T>
// clone: T -> T (where T: Clone)
```

## Codegen to lIR
```lisp
; (share x) compiles to:
(rc-alloc <type>)
(store x (rc-ptr result))

; (clone x) compiles to:
; Deep copy - depends on type
; For primitives: just copy
; For structs: allocate new + copy fields
; For rc: rc-clone (refcount++)
```

## Ownership Implications
- share creates Rc<T>, multiple owners
- clone creates fresh owned value
- Shared values immutable (use atoms for mutation)

## Acceptance Criteria
- [ ] share creates reference-counted value
- [ ] clone creates deep copy
- [ ] Shared values are immutable
- [ ] Proper refcount management
- [ ] Works with borrow checker
