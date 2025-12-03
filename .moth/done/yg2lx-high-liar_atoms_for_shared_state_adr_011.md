## Summary
Implement atoms for thread-safe mutable state following Clojure's model.

## Syntax
```lisp
(atom initial-value)     ; Create atomic cell
(swap! atom fn)          ; Atomic update: atom = fn(current)
(reset! atom value)      ; Atomic set
@atom                    ; Atomic read (deref)
(compare-and-set! atom old new)  ; CAS
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Atom(Box<Spanned<Expr>>),           // (atom x)
    Swap(Box<Spanned<Expr>>, Box<Spanned<Expr>>),  // (swap! atom fn)
    Reset(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (reset! atom val)
    AtomDeref(Box<Spanned<Expr>>),      // @atom
    CompareAndSet {                      // (compare-and-set! atom old new)
        atom: Box<Spanned<Expr>>,
        old: Box<Spanned<Expr>>,
        new: Box<Spanned<Expr>>,
    },
}
```

## Lexer Additions
```rust
'@' => TokenKind::At,        // For @atom deref
"swap!" => TokenKind::Swap,
"reset!" => TokenKind::Reset,
"compare-and-set!" => TokenKind::CAS,
```

## Type Checking
```rust
// atom creates Atom<T>
// swap! takes Atom<T> and (T -> T), returns T
// reset! takes Atom<T> and T, returns T
// @atom returns T
// compare-and-set! takes Atom<T>, T, T, returns bool
```

## Codegen to lIR
```lisp
; (atom 0) compiles to:
(rc-alloc i64)  ; Atoms are ref-counted for sharing

; (swap! a inc) compiles to:
; CAS loop:
(block swap_loop
  (let ((current (load i64 (rc-ptr a))))
    (let ((new (call @inc current)))
      (br (cmpxchg a current new) swap_done swap_loop))))

; @atom compiles to:
(load i64 (rc-ptr a))
```

## Test Cases
```lisp
; Basic atom
(let ((a (atom 0)))
  (swap! a inc)
  @a)  ; => 1

; In plet
(let ((counter (atom 0)))
  (plet ((a (swap! counter inc))
         (b (swap! counter inc)))
    @counter))  ; => 2 (both swaps complete)
```

## Acceptance Criteria
- [ ] atom creates atomic cell
- [ ] swap! performs atomic update
- [ ] reset! performs atomic set
- [ ] @atom reads atomically
- [ ] compare-and-set! works
- [ ] Thread-safe in plet
