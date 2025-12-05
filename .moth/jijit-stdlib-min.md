# stdlib: min

**Priority:** P1  
**Category:** stdlib/arithmetic  
**Dependencies:** none  
**Order:** 180

## Description

Returns the smaller of two values.

## Implementation

```lisp
(defun min (a b) (if (< a b) a b))
```

## Tests

```lisp
(min 1 2)  ; => 1
(min 5 3)  ; => 3
(min 4 4)  ; => 4
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
