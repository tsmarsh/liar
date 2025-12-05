# stdlib: neg?

**Priority:** P1  
**Category:** stdlib/predicates  
**Dependencies:** none  
**Order:** 130

## Description

Returns true if x is negative (less than zero).

## Implementation

```lisp
(defun neg? (x) (< x 0))
```

## Tests

```lisp
(neg? -1)  ; => true
(neg? 0)   ; => false
(neg? 1)   ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
