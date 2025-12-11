# Destructuring In Let Bindings

Add destructuring support in `let` bindings, following Clojure's approach.

## Syntax

```lisp
;; Struct destructuring
(let (((Point x y) some-point))
  (+ x y))

;; Equivalent to:
(let ((p some-point))
  (+ (. p x) (. p y)))
```

## Implementation

This is syntax sugar - the parser expands destructuring patterns into field access:

1. Parse destructuring pattern `(StructName field1 field2 ...)`
2. Generate a fresh temp variable for the value
3. Generate field access expressions for each bound name

## Scope

- Struct destructuring only (no tuple/vector destructuring initially)
- Only in `let` bindings (not function parameters initially)
- No nested destructuring initially

## Not included

- Full pattern matching with guards
- Tuple destructuring
- Vector destructuring
- Wildcard patterns (`_`)

These can be added later if needed.
