# stdlib: cons

**Tier:** P4
**Category:** stdlib/lists
**Dependencies:** runtime struct allocation

## Description

**BLOCKED: Requires cons cell runtime**

Creates a pair (cons cell) with car=a and cdr=b. Needs runtime memory allocation for pairs.

## Implementation

```lisp
;; BLOCKED: Needs struct instantiation at runtime
;; (defstruct Pair (car cdr))
;; (defun cons (a b) (Pair a b))
```

## Tests

```lisp
(cons 1 2)           ; => (1 . 2)
(cons 1 (cons 2 nil)) ; => (1 2)
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
