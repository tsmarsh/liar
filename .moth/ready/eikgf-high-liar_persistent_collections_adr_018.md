## Summary
Implement persistent (immutable, structural sharing) collections:
vector, map, list.

## Syntax
```lisp
[1 2 3]           ; Persistent vector
{:a 1 :b 2}       ; Persistent map
'(1 2 3)          ; List (linked)
```

## Operations
```lisp
; Vector
(conj [1 2] 3)         ; => [1 2 3]
(get [1 2 3] 0)        ; => 1
(assoc [1 2 3] 1 99)   ; => [1 99 3]
(count [1 2 3])        ; => 3
(subvec [1 2 3 4] 1 3) ; => [2 3]

; Map
(assoc {:a 1} :b 2)    ; => {:a 1 :b 2}
(dissoc {:a 1 :b 2} :a); => {:b 2}
(get {:a 1} :a)        ; => 1
(keys {:a 1 :b 2})     ; => (:a :b)
(vals {:a 1 :b 2})     ; => (1 2)

; List
(cons 0 '(1 2))        ; => (0 1 2)
(first '(1 2 3))       ; => 1
(rest '(1 2 3))        ; => (2 3)
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Vector(Vec<Spanned<Expr>>),
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    List(Vec<Spanned<Expr>>),
    Keyword(String),  // :foo
}
```

## Lexer Additions
```rust
'[' => TokenKind::LBracket,
']' => TokenKind::RBracket,
'{' => TokenKind::LBrace,
'}' => TokenKind::RBrace,
':' followed by symbol => TokenKind::Keyword(s)
```

## Implementation Options
1. Use im-rs crate (Rust persistent collections)
2. Build custom HAMTs/RRB-trees
3. Start simple (copy-on-write), optimize later

## Type System
```rust
Ty::Vector(Box<Ty>),      // [T]
Ty::Map(Box<Ty>, Box<Ty>), // {K V}
Ty::List(Box<Ty>),         // '(T)
Ty::Keyword,               // :keyword
```

## Acceptance Criteria
- [ ] Vector literal parses and evaluates
- [ ] Map literal parses and evaluates  
- [ ] List literal parses and evaluates
- [ ] Keywords parse (:foo)
- [ ] conj, get, assoc work
- [ ] Structural sharing (verify no deep copies)
