# stdlib: max

**Tier:** P1
**Category:** stdlib/arithmetic
**Dependencies:** none

## Description

Returns the larger of two values.

## Implementation

```lisp
(defun max (a b) (if (> a b) a b))
```

## Tests

```lisp
(max 1 2)  ; => 2
(max 5 3)  ; => 5
(max 4 4)  ; => 4
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
