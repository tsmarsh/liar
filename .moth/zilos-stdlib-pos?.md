# stdlib: pos?

**Priority:** P1  
**Category:** stdlib/predicates  
**Dependencies:** none  
**Order:** 120

## Description

Returns true if x is positive (greater than zero).

## Implementation

```lisp
(defun pos? (x) (> x 0))
```

## Tests

```lisp
(pos? 1)   ; => true
(pos? 0)   ; => false
(pos? -1)  ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
