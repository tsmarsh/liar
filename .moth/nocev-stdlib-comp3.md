# stdlib: comp3

**Priority:** P3  
**Category:** stdlib/higher-order  
**Dependencies:** none  
**Order:** 360

## Description

Composition of three functions: (f (g (h x))).

## Implementation

```lisp
(defun comp3 (f g h)
  (fn (x) (f (g (h x)))))
```

## Tests

```lisp
(let ((f (comp3 square inc inc)))
  (f 3))  ; => 25
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
