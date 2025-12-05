# stdlib: reduce

**Tier:** P4
**Category:** stdlib/collections
**Dependencies:** cons, car, cdr, nil?

## Description

**BLOCKED: Requires list/vector iteration**

Reduces coll to single value by applying (f acc item) left to right.

## Implementation

```lisp
;; BLOCKED: Needs cons/car/cdr or vector iteration  
;; (defun reduce (f init coll)
;;   (if (nil? coll)
;;       init
;;       (reduce f (f init (car coll)) (cdr coll))))
```

## Tests

```lisp
(reduce + 0 [1 2 3 4 5])  ; => 15
(reduce * 1 [1 2 3 4])    ; => 24
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
