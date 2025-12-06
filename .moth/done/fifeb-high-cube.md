# stdlib: cube

**Tier:** P1
**Category:** stdlib/arithmetic
**Dependencies:** none

## Description

Returns x cubed (x * x * x).

## Implementation

```lisp
(defun cube (x) (* x (* x x)))
```

## Tests

```lisp
(cube 2)   ; => 8
(cube 3)   ; => 27
(cube -2)  ; => -8
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
