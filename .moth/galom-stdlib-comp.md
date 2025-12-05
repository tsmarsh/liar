# stdlib: comp

**Priority:** P2  
**Category:** stdlib/higher-order  
**Dependencies:** none  
**Order:** 310

## Description

Returns composition of two functions: (comp f g) returns fn that does (f (g x)).

## Implementation

```lisp
(defun comp (f g)
  (fn (x) (f (g x))))
```

## Tests

```lisp
(let ((add1-then-square (comp square inc)))
  (add1-then-square 4))  ; => 25
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
