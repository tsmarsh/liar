## Summary
Implement Rust-style iterators instead of pervasive laziness.
Iterators are explicit, have clear ownership, and avoid space leaks.

## Why Not Laziness?
- Unpredictable performance (when does work happen?)
- Space leaks (hold head, keep whole seq in memory)
- Ownership complexity (who owns the thunk?)
- Rust rejected it for good reasons

## Iterator Protocol
```lisp
(defprotocol Iter
  (next [self] "Returns {:some [value next-iter]} or :none."))
```

## Usage
```lisp
; Create iterator from collection
(-> [1 2 3 4 5]
    (iter)                    ; Create iterator
    (iter/map square)         ; Lazy transform
    (iter/filter even?)       ; Lazy filter  
    (iter/take 3)             ; Lazy take
    (collect))                ; Materialize HERE

; Infinite sequences via iterator
(-> (iter/range)              ; 0, 1, 2, ...
    (iter/map square)
    (iter/take 10)
    (collect))                ; [0 1 4 9 16 25 36 49 64 81]
```

## Core Iterator Functions
```lisp
;; Creation
(iter coll)                   ; Collection -> Iter
(iter/range)                  ; Infinite: 0, 1, 2, ...
(iter/range n)                ; 0 to n-1
(iter/range start end)        ; start to end-1
(iter/repeat x)               ; Infinite: x, x, x, ...
(iter/iterate f x)            ; x, (f x), (f (f x)), ...

;; Transforms (lazy, return new iterator)
(iter/map f it)
(iter/filter pred it)
(iter/take n it)
(iter/drop n it)
(iter/take-while pred it)
(iter/drop-while pred it)
(iter/enumerate it)           ; (0, a), (1, b), ...
(iter/zip it1 it2)
(iter/chain it1 it2)
(iter/flatten it)             ; Iter<Iter<T>> -> Iter<T>
(iter/flat-map f it)          ; map then flatten

;; Consumers (force evaluation)
(collect it)                  ; -> vector
(iter/reduce f init it)
(iter/for-each f it)          ; side effects
(iter/count it)
(iter/sum it)
(iter/any? pred it)
(iter/all? pred it)
(iter/find pred it)
(iter/nth it n)
(iter/last it)
```

## Ownership Semantics
```lisp
; Iterator is consumed on use
(let ((it (iter [1 2 3])))
  (let ((it2 (iter/map square it)))  ; it moved into it2
    (collect it2)))                   ; it2 consumed

; Can't use iterator twice
(let ((it (iter [1 2 3])))
  (collect it)
  (collect it))  ; ERROR: use after move
```

## Comparison: Lazy vs Iterator
```lisp
;; Lazy (NOT doing this)
(take 5 (map square (naturals)))  ; When computed? Who knows.

;; Iterator (doing this)
(-> (iter/range)
    (iter/map square)
    (iter/take 5)
    (collect))  ; Computed HERE. Clear.
```

## Integration with Seq
Iterators are NOT seqs. They're consumed on use.
Seq functions work on collections directly (eager).
Iterator functions are in `iter/` namespace (lazy).
```lisp
; Seq (eager, on collections)
(map f [1 2 3])        ; Returns new vector immediately

; Iter (lazy, explicit)
(-> [1 2 3]
    (iter)
    (iter/map f)
    (collect))         ; Returns vector when collected
```

## Acceptance Criteria
- [ ] Iter protocol defined
- [ ] iter creates iterator from collection
- [ ] iter/map, filter, take, drop work
- [ ] collect materializes to vector
- [ ] iter/range for infinite sequences
- [ ] Proper ownership (consumed on use)
- [ ] No space leaks
