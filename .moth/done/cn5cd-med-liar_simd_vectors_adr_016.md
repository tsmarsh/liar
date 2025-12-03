## Summary
Implement SIMD vector literals and operations for data parallelism.

## Syntax
```lisp
<<1 2 3 4>>              ; v4 i64 (inferred)
<<1.0 2.0 3.0 4.0>>      ; v4 f64 (inferred)
<i8<1 2 3 4>>            ; v4 i8 (explicit type)
<f32<1.0 2.0 3.0 4.0>>   ; v4 f32 (explicit type)
```

## Operations
```lisp
; Arithmetic broadcasts to all lanes
(+ <<1 2 3 4>> <<5 6 7 8>>)   ; => <<6 8 10 12>>
(* 2 <<1 2 3 4>>)             ; => <<2 4 6 8>>

; Lane access
(lane vec idx)                 ; Extract single lane
(with-lane vec idx val)        ; Return vec with lane updated

; Horizontal operations
(hsum <<1 2 3 4>>)            ; => 10 (horizontal sum)
(hmin <<1 2 3 4>>)            ; => 1
(hmax <<1 2 3 4>>)            ; => 4
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    SimdVector(Vec<Spanned<Expr>>),           // <<...>>
    SimdVectorTyped(String, Vec<Spanned<Expr>>), // <i8<...>>
}
```

## Lexer
```rust
"<<" => TokenKind::DoubleLAngle,
">>" => TokenKind::DoubleRAngle,
// <i8< needs careful parsing
```

## Type System
```rust
Ty::SimdVector(Box<Ty>, usize),  // <N x T>
```

## Codegen to lIR
```lisp
; <<1 2 3 4>> compiles to:
(vector <4 x i64> (i64 1) (i64 2) (i64 3) (i64 4))

; (+ a b) on vectors compiles to:
(add a b)  ; lIR add works on vectors
```

## Constraints
- Vector width must be power of 2
- All elements same type
- Operations maintain lane correspondence

## Acceptance Criteria
- [ ] <<...>> syntax parses
- [ ] <type<...>> syntax parses
- [ ] Arithmetic works on vectors
- [ ] Scalar broadcast works
- [ ] Horizontal operations work
- [ ] Maps to lIR vector types
