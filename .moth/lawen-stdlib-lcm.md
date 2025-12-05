# stdlib: lcm

**Priority:** P2  
**Category:** stdlib/math  
**Dependencies:** gcd  
**Order:** 260

## Description

Least common multiple.

## Implementation

```lisp
(defun lcm (a b)
  (/ (* a b) (gcd a b)))
```

## Tests

```lisp
(lcm 4 6)   ; => 12
(lcm 3 5)   ; => 15
(lcm 8 12)  ; => 24
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
