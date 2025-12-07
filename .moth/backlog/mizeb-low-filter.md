# stdlib: filter

**Tier:** P4
**Category:** stdlib/collections
**Dependencies:** cons, car, cdr, nil?

## Description

**BLOCKED: Requires list/vector iteration**

Returns collection of elements where (pred x) is true.

## Implementation

```lisp
;; BLOCKED: Needs cons/car/cdr or vector iteration
;; (defun filter (pred coll)
;;   (if (nil? coll)
;;       nil
;;       (if (pred (car coll))
;;           (cons (car coll) (filter pred (cdr coll)))
;;           (filter pred (cdr coll)))))
```

## Tests

```lisp
(filter even? [1 2 3 4 5])  ; => [2 4]
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
