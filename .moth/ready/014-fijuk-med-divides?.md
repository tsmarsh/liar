# stdlib: divides?

**Tier:** P2
**Category:** stdlib/predicates
**Dependencies:** none

## Description

Returns true if d divides n evenly (n mod d = 0).

## Implementation

```lisp
(defun divides? (d n) (= 0 (rem n d)))
```

## Tests

```lisp
(divides? 2 4)  ; => true
(divides? 2 5)  ; => false
(divides? 3 9)  ; => true
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
