# ConvVector - Mutable Growable Array

**Priority:** med
**Category:** lib/collections
**Dependencies:** arrays, protocols

## Summary

Conventional mutable vector (like Rust's `Vec` or Java's `ArrayList`). O(1) amortized append, O(1) random access. Uses protocol-based design for consistency with the rest of the language.

## Design Decisions

**Protocol-based approach:** Following the same pattern as `seq.liar`, we define protocols for mutable collection operations and extend them for MutVector. This gives us:
- Polymorphism (any type can implement these protocols)
- Consistency with persistent collections
- Clear separation of interface from implementation

**Array-based storage:** Since we don't have struct field mutation, MutVector uses a single array where:
- `data[0]` = current length
- `data[1]` = capacity
- `data[2+]` = elements

This allows true O(1) mutation via `aset`.

**Growth returns new vector:** When capacity is exceeded, `mv-push!` returns a new (grown) vector. Caller must use the returned value:
```lisp
(let ((v (mv-push! v 42))) ...)  ;; Always use returned value
```

## Protocols

```lisp
;; Mutable indexed access
(defprotocol MutIndexed
  (mv-nth [self idx])       ;; get element at index
  (mv-set! [self idx val])) ;; set element, returns self

;; Mutable collection info
(defprotocol MutCounted
  (mv-count [self])         ;; current element count
  (mv-empty? [self]))       ;; true if no elements

;; Growable collection
(defprotocol MutGrowable
  (mv-push! [self val])     ;; append, returns (possibly new) self
  (mv-pop! [self]))         ;; remove last, returns value
```

## Data Structure

```lisp
;; MutVector wraps an array: [len, cap, elem0, elem1, ...]
(defstruct MutVector (data: ptr))
```

## Implementation

**Location:** `lib/mut-vector.liar`

Uses protocol dispatch for all operations. The underlying array stores metadata in slots 0-1 and elements in slots 2+.

## API

```lisp
;; Construction
(mut-vector)              ;; empty with default capacity
(mut-vector-cap n)        ;; empty with capacity n

;; Access via MutIndexed protocol
(mv-nth v idx)            ;; O(1)
(mv-set! v idx val)       ;; O(1), returns v

;; Info via MutCounted protocol
(mv-count v)              ;; O(1)
(mv-empty? v)             ;; O(1)

;; Mutation via MutGrowable protocol
(mv-push! v val)          ;; O(1) amortized, returns v (may be new)
(mv-pop! v)               ;; O(1), returns popped value

;; Convenience (non-protocol)
(mv-first v)              ;; first element
(mv-last v)               ;; last element
(mv-capacity v)           ;; allocated capacity
```

## Tests

```lisp
;; Basic operations
(let ((v (mut-vector)))
  (let ((v (mv-push! v 10)))
    (let ((v (mv-push! v 20)))
      (+ (mv-nth v 0) (mv-nth v 1)))))  ;; => 30

;; Growth triggers reallocation
(let ((v (mut-vector-cap 2)))
  (let ((v (mv-push! v 1)))
    (let ((v (mv-push! v 2)))
      (let ((v (mv-push! v 3)))  ;; triggers growth
        (mv-count v)))))  ;; => 3
```

## Acceptance Criteria

- [x] Define MutIndexed, MutCounted, MutGrowable protocols
- [x] MutVector struct wrapping array
- [x] Extend protocols for MutVector
- [x] Dynamic heap-array allocation for growth
- [ ] Feature tests for mutable vector operations
- [ ] `<[...]>` syntax generates MutVector (future)
