# stdlib: factorial

**Tier:** P2
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns n! (n factorial). n must be non-negative.

## Implementation

```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

## Tests

```lisp
(factorial 0)  ; => 1
(factorial 1)  ; => 1
(factorial 5)  ; => 120
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
