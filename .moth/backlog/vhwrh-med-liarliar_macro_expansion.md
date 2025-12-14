# liarliar: Macro Expansion

Create `lib/liarliar/expand.liar` - expand macros by pattern matching on list structure.

## Overview

Macros receive quoted forms (Cons cells) and return new forms. True homoiconicity.

## Main Expander

```clojure
(defun expand (ctx form)
  (cond
    ((not (cons? form)) form)
    ((quote? form) (cadr form))
    ((quasiquote? form) (expand-qq ctx (cadr form)))
    ((defmacro? form) (register-macro! ctx form))
    ((macro-call? ctx form) (expand-macro ctx form))
    (true (map-form (fn (x) (expand ctx x)) form))))
```

## Quasiquote Expansion

```clojure
(defun expand-qq (ctx form)
  (cond
    ((unquote? form) (expand ctx (cadr form)))
    ((unquote-splicing? form) (error "splice at top level"))
    ((cons? form) (expand-qq-list ctx form))
    (true (list 'quote form))))

(defun expand-qq-list (ctx form)
  (if (nil? form)
      nil
      (let ((head (car form))
            (tail (cdr form)))
        (if (unquote-splicing? head)
            (list 'append (cadr head) (expand-qq-list ctx tail))
            (list 'cons (expand-qq ctx head) (expand-qq-list ctx tail))))))
```

## Macro Registration

```clojure
(defun register-macro! (ctx form)
  ;; (defmacro name (params) body)
  (let ((name (cadr form))
        (params (caddr form))
        (body (cadddr form)))
    (add-macro ctx name params body)))
```

## Functions to Implement

- `expand` - Main dispatcher
- `expand-qq` - Quasiquote handling
- `expand-qq-list` - List inside quasiquote
- `register-macro!` - Store macro definition
- `expand-macro` - Apply macro to arguments
- `quote?`, `quasiquote?`, `unquote?`, `unquote-splicing?` - Predicates
- `defmacro?`, `macro-call?` - Detection

## Dependencies

- `lib/liarliar/value.liar` - Tagged value predicates
- `lib/liarliar/symbols.liar` - Symbol comparison

## Test Cases

- `'(1 2 3)` -> list unchanged
- `` `(a ,b c)`` with b=2 -> `(a 2 c)`
- `` `(a ,@xs c)`` with xs=(1 2) -> `(a 1 2 c)`
- `(defmacro unless ...)` then `(unless cond a b)` expands
