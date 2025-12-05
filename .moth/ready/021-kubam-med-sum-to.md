# stdlib: sum-to

**Tier:** P2
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns sum of 1 + 2 + ... + n.

## Implementation

```lisp
(defun sum-to (n)
  (if (<= n 0)
      0
      (+ n (sum-to (- n 1)))))
```

## Tests

```lisp
(sum-to 0)    ; => 0
(sum-to 10)   ; => 55
(sum-to 100)  ; => 5050
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
