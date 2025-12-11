# stdlib: map

**Tier:** P4
**Category:** stdlib/collections
**Dependencies:** cons, car, cdr, nil?

## Description

**BLOCKED: Requires list/vector iteration**

Applies f to each element of coll, returns new collection.

## Implementation

```lisp
;; BLOCKED: Needs cons/car/cdr or vector iteration
;; (defun map (f coll)
;;   (if (nil? coll)
;;       nil
;;       (cons (f (car coll)) (map f (cdr coll)))))
```

## Tests

```lisp
(map inc [1 2 3])      ; => [2 3 4]
(map square [1 2 3])   ; => [1 4 9]
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
