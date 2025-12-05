# stdlib: sign

**Priority:** P1  
**Category:** stdlib/arithmetic  
**Dependencies:** none  
**Order:** 210

## Description

Returns -1 if x < 0, 0 if x = 0, 1 if x > 0.

## Implementation

```lisp
(defun sign (x)
  (if (< x 0) -1
      (if (> x 0) 1 0)))
```

## Tests

```lisp
(sign -5)  ; => -1
(sign 0)   ; => 0
(sign 5)   ; => 1
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
