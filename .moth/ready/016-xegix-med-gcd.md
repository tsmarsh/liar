# stdlib: gcd

**Tier:** P2
**Category:** stdlib/math
**Dependencies:** none

## Description

Greatest common divisor using Euclidean algorithm.

## Implementation

```lisp
(defun gcd (a b)
  (if (= b 0)
      a
      (gcd b (rem a b))))
```

## Tests

```lisp
(gcd 12 8)   ; => 4
(gcd 17 5)   ; => 1
(gcd 100 25) ; => 25
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
