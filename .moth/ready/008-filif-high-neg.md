# stdlib: neg

**Tier:** P1
**Category:** stdlib/arithmetic
**Dependencies:** none

## Description

Returns the negation of x.

## Implementation

```lisp
(defun neg (x) (- 0 x))
```

## Tests

```lisp
(neg 5)    ; => -5
(neg -5)   ; => 5
(neg 0)    ; => 0
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
