# stdlib: flip

**Priority:** P2  
**Category:** stdlib/higher-order  
**Dependencies:** none  
**Order:** 320

## Description

Returns a function with arguments flipped: (flip f) returns (fn (a b) (f b a)).

## Implementation

```lisp
(defun flip (f)
  (fn (a b) (f b a)))
```

## Tests

```lisp
((flip -) 3 10)  ; => 7  (computes 10 - 3)
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
