# ConvVector - Mutable Growable Array

**Priority:** med
**Category:** lib/collections
**Dependencies:** arrays, struct instantiation

## Summary

Conventional mutable vector (like Rust's `Vec` or Java's `ArrayList`). O(1) amortized append, O(1) random access. For cases where persistence isn't needed and mutation is more efficient.

## Syntax
```lisp
<[1 2 3]>           ;; mutable vector literal
```

## Data Structure
```lisp
(defstruct MutVector
  (len: i64)        ;; current element count
  (cap: i64)        ;; allocated capacity
  (data))           ;; backing array
```

## API
```lisp
;; Construction
(mut-vector)            ;; empty, default capacity
(mut-vector cap)        ;; empty with initial capacity
<[a b c]>               ;; literal syntax

;; Access - O(1)
(nth! v idx)            ;; get element (bounds checked)
(first! v)
(last! v)

;; Mutation - O(1) amortized
(push! v val)           ;; append, grows if needed
(pop! v)                ;; remove and return last
(set! v idx val)        ;; update at index

;; Info - O(1)
(len v)
(capacity v)
(empty?! v)

;; Bulk
(clear! v)              ;; remove all elements
(reserve! v n)          ;; ensure capacity >= n
(shrink! v)             ;; shrink capacity to len

;; Conversion
(freeze v)              ;; -> PersistentVector (transfers ownership)
```

## Implementation

**Location:** `lib/collections/mut-vector.liar`
```lisp
(def DEFAULT-CAP 16)
(def GROWTH-FACTOR 2)

(defstruct MutVector (len cap data))

(defun mut-vector ()
  (MutVector len: 0 cap: DEFAULT-CAP data: (make-array DEFAULT-CAP)))

(defun mut-vector-with-cap (cap)
  (MutVector len: 0 cap: cap data: (make-array cap)))

;; Access
(defun nth! (v idx)
  (if (and (>= idx 0) (< idx (. v len)))
    (aget (. v data) idx)
    (error "Index out of bounds")))

(defun first! (v) (nth! v 0))
(defun last! (v) (nth! v (- (. v len) 1)))

;; Mutation
(defun push! (v val)
  (when (= (. v len) (. v cap))
    (grow! v))
  (aset (. v data) (. v len) val)
  (set! (. v len) (+ (. v len) 1))
  v)

(defun pop! (v)
  (if (= (. v len) 0)
    (error "Cannot pop empty vector")
    (do
      (set! (. v len) (- (. v len) 1))
      (aget (. v data) (. v len)))))

(defun set-at! (v idx val)
  (if (and (>= idx 0) (< idx (. v len)))
    (aset (. v data) idx val)
    (error "Index out of bounds"))
  v)

;; Growth
(defun grow! (v)
  (let ((new-cap (* (. v cap) GROWTH-FACTOR))
        (new-data (make-array new-cap)))
    (array-copy-into (. v data) new-data (. v len))
    (set! (. v data) new-data)
    (set! (. v cap) new-cap)))

(defun reserve! (v n)
  (when (> n (. v cap))
    (let ((new-data (make-array n)))
      (array-copy-into (. v data) new-data (. v len))
      (set! (. v data) new-data)
      (set! (. v cap) n)))
  v)

;; Info
(defun len (v) (. v len))
(defun capacity (v) (. v cap))
(defun empty?! (v) (= (. v len) 0))

;; Clear
(defun clear! (v)
  (set! (. v len) 0)
  v)

;; Freeze to persistent (move semantics)
(defun freeze (v)
  (let ((pv (reduce conj (vector) (mut-vec-to-list v))))
    (clear! v)  ;; invalidate mutable
    pv))
```

## Ownership Model

- Single owner (not `share`d by default)
- Mutation requires ownership or `&mut` reference
- `freeze` transfers ownership to persistent vector
- No structural sharing - full copies on clone

## Tests
```lisp
;; Basic mutation
(let ((v (mut-vector)))
  (push! v 1)
  (push! v 2)
  (push! v 3)
  (assert (= (len v) 3))
  (assert (= (nth! v 1) 2))
  (assert (= (pop! v) 3))
  (assert (= (len v) 2)))

;; Growth
(let ((v (mut-vector-with-cap 2)))
  (push! v 1)
  (push! v 2)
  (push! v 3)  ;; triggers growth
  (assert (>= (capacity v) 3))
  (assert (= (nth! v 2) 3)))

;; Set
(let ((v <[1 2 3]>))
  (set-at! v 1 99)
  (assert (= (nth! v 1) 99)))

;; Freeze
(let ((v <[1 2 3]>))
  (let ((pv (freeze v)))
    (assert (= (count pv) 3))
    (assert (= (nth pv 0) 1))
    (assert (empty?! v))))  ;; mutable now empty
```

## Acceptance Criteria

- [ ] `MutVector` struct with len/cap/data
- [ ] `push!`/`pop!` with automatic growth
- [ ] `nth!`/`set-at!` with bounds checking
- [ ] `reserve!`/`clear!`/`shrink!`
- [ ] `freeze` to PersistentVector
- [ ] Ownership: mutation requires ownership
- [ ] `<[...]>` syntax parses to `ConvVector`
