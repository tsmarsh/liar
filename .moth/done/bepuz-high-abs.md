# stdlib: abs

**Tier:** P1
**Category:** stdlib/arithmetic
**Dependencies:** none

## Description

Returns the absolute value of x.

## Implementation

```lisp
(defun abs (x) (if (< x 0) (- 0 x) x))
```

## Tests

```lisp
(abs 5)    ; => 5
(abs -5)   ; => 5
(abs 0)    ; => 0
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
