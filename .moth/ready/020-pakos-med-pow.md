# stdlib: pow

**Tier:** P2
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns base raised to the power exp. exp must be non-negative integer.

## Implementation

```lisp
(defun pow (base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))))
```

## Tests

```lisp
(pow 2 0)   ; => 1
(pow 2 10)  ; => 1024
(pow 3 4)   ; => 81
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
