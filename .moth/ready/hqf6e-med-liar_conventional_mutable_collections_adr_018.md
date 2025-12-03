## Summary
Implement conventional (mutable, O(1) access) collections alongside
persistent ones.

## Syntax
```lisp
<[1 2 3]>         ; Conventional vector
<{:a 1 :b 2}>     ; Conventional map
```

## Operations
```lisp
; Mutable operations (with !)
(push! vec item)       ; Append in place
(set! vec idx val)     ; Update in place
(put! map key val)     ; Insert in place
(remove! map key)      ; Remove in place

; Same read operations as persistent
(get vec idx)
(count vec)
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    ConvVector(Vec<Spanned<Expr>>),
    ConvMap(Vec<(Spanned<Expr>, Spanned<Expr>)>),
}
```

## Lexer
```rust
// <[ starts conventional vector
// <{ starts conventional map
// Need lookahead for < followed by [ or {
```

## Ownership
- Conventional collections are owned
- Mutations require &mut
- Cannot be shared without atoms/rc

## Type System
```rust
Ty::ConvVector(Box<Ty>),    // <[T]>
Ty::ConvMap(Box<Ty>, Box<Ty>), // <{K V}>
```

## Implementation
Use Rust's Vec and HashMap under the hood.

## Acceptance Criteria
- [ ] <[...]> syntax parses
- [ ] <{...}> syntax parses
- [ ] Mutable operations work
- [ ] Ownership rules apply
- [ ] Performance: O(1) access
