# Macros (defmacro)

**Priority:** P0 (Critical - blocks async I/O)
**Category:** liar/macros
**Dependencies:** none

## Summary

Implement Lisp-style macros with `defmacro`, quasiquote syntax, and `gensym` for hygienic macro expansion.

## Current State

No macro support exists. No `macros.rs` or `expand.rs` files.

## Requirements

### defmacro
- Define macros that transform code at compile time
- Macros receive unevaluated syntax as arguments
- Macros return syntax to be compiled

### Quasiquote
- Backtick (`) for quasiquote template
- Comma (,) for unquote (insert value)
- Comma-at (,@) for unquote-splicing (insert list elements)

### gensym
- Generate unique symbols for hygienic macros
- Prevents name capture/collision

### Macro Expansion
- Expand macros before type inference
- Support nested macro calls
- Error on undefined macros

## Implementation

**New files:**
- `liar/src/macros.rs` - macro definitions and registry
- `liar/src/expand.rs` - macro expansion pass

### AST additions to `ast.rs`:
```rust
pub enum Item {
    // ... existing ...
    Defmacro(Defmacro),
}

pub struct Defmacro {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>,
    pub body: Spanned<Expr>,
}

pub enum Expr {
    // ... existing ...
    Quasiquote(Box<Spanned<Expr>>),
    Unquote(Box<Spanned<Expr>>),
    UnquoteSplicing(Box<Spanned<Expr>>),
    Gensym(Option<String>),
}
```

### Expansion pass (before type inference):
```rust
// expand.rs
pub fn expand_macros(program: &Program, macros: &MacroRegistry) -> Result<Program> {
    // Walk AST, replace macro calls with expanded code
}
```

## Tests

```lisp
;; Basic macro
(defmacro when (cond &rest body)
  `(if ,cond (do ,@body) nil))

(when true
  (print "yes"))
;; Expands to: (if true (do (print "yes")) nil)

;; Macro with gensym (hygienic)
(defmacro swap (a b)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

;; Nested quasiquote
(defmacro def-pred (name test)
  `(defun ,name (x) (,test x)))

(def-pred positive? (fn (n) (> n 0)))
```

## Acceptance Criteria

- [ ] `defmacro` parses and registers macro
- [ ] Quasiquote: `` `(a ,x ,@xs) `` works
- [ ] `gensym` generates unique names
- [ ] Macro expansion runs before type inference
- [ ] `(defmacro when (c &rest body) `(if ,c (do ,@body) nil))` works
- [ ] Tests added to liar-cert
