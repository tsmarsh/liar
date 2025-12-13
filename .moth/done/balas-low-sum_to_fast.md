# stdlib: sum-to-fast

**Tier:** P3
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns sum of 1 + 2 + ... + n using closed-form formula.

## Implementation

```lisp
(defun sum-to-fast (n)
  (/ (* n (+ n 1)) 2))
```

## Tests

```lisp
(sum-to-fast 100)    ; => 5050
(sum-to-fast 1000)   ; => 500500
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
