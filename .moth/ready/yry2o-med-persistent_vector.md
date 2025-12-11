# PersistentVector - 32-way Branching Trie

**Priority:** high
**Category:** lib/collections
**Dependencies:** bitops (done), arrays, trqhk (br/phi)

## Blocked By

- **trqhk** - Recursion requires proper `if` with br/phi
- Array operations need runtime support

## Summary

Implement Bagwell-style persistent vector (32-way trie with tail optimization). O(log32 n) ≈ O(1) access/update with structural sharing.

## Data Structure
```lisp
(defstruct PersistentVector
  (count: i64)      ;; element count
  (shift: i64)      ;; tree depth * 5
  (root)            ;; tree root (VecNode or nil)
  (tail))           ;; last <=32 elements (array)

(defstruct VecNode
  (array))          ;; 32-element array
```

## API
```lisp
;; Construction
(vector)              ;; empty
(vector a b c)        ;; from elements
(vec lst)             ;; from list

;; Access - O(log32 n)
(nth v idx)
(first v)
(peek v)              ;; last element, O(1) via tail

;; Update - O(log32 n), returns new vector
(conj v val)          ;; append
(assoc v idx val)     ;; update at index
(pop v)               ;; remove last

;; Info - O(1)
(count v)
(empty? v)

;; Slicing
(subvec v start end)
```

## Implementation

**Location:** `lib/collections/vector.liar`
```lisp
(def BITS 5)
(def WIDTH 32)
(def MASK 0x1f)

(defstruct PersistentVector (count shift root tail))
(defstruct VecNode (array))

(def EMPTY-VEC (PersistentVector count: 0 shift: BITS root: nil tail: (make-array 0)))

;; Index within level
(defun vec-index (idx shift)
  (bit-and (bit-shift-right idx shift) MASK))

;; Where tail starts
(defun tail-offset (v)
  (if (< (. v count) WIDTH)
    0
    (bit-shift-left (bit-shift-right (- (. v count) 1) BITS) BITS)))

;; Find array containing idx
(defun array-for (v idx)
  (if (>= idx (tail-offset v))
    (. v tail)
    (let ((node (. v root))
          (shift (. v shift)))
      (while (> shift 0)
        (set! node (aget (. node array) (vec-index idx shift)))
        (set! shift (- shift BITS)))
      (. node array))))

(defun nth (v idx)
  (if (and (>= idx 0) (< idx (. v count)))
    (aget (array-for v idx) (bit-and idx MASK))
    nil))

(defun count (v) (. v count))
(defun empty? (v) (= (. v count) 0))
(defun peek (v)
  (if (> (. v count) 0)
    (aget (. v tail) (- (. v count) (tail-offset v) 1))
    nil))

;; Conj - append to tail or overflow into tree
(defun conj (v val)
  (let ((cnt (. v count)))
    (if (< (- cnt (tail-offset v)) WIDTH)
      ;; Room in tail
      (share (PersistentVector
        count: (+ cnt 1)
        shift: (. v shift)
        root: (. v root)
        tail: (array-append (. v tail) val)))
      ;; Overflow - push tail to tree
      (let ((tail-node (share (VecNode array: (. v tail))))
            (new-root (push-tail v (. v shift) (. v root) tail-node))
            (overflow (> (bit-shift-right cnt BITS) (bit-shift-left 1 (. v shift)))))
        (share (PersistentVector
          count: (+ cnt 1)
          shift: (if overflow (+ (. v shift) BITS) (. v shift))
          root: (if overflow
                  (share (VecNode array: (array-of (. v root) new-root)))
                  new-root)
          tail: (array-of val)))))))

;; Assoc - path copy to update
(defun assoc (v idx val)
  (if (= idx (. v count))
    (conj v val)
    (if (>= idx (tail-offset v))
      ;; In tail
      (share (PersistentVector
        count: (. v count)
        shift: (. v shift)
        root: (. v root)
        tail: (array-set-copy (. v tail) (bit-and idx MASK) val)))
      ;; In tree - path copy
      (share (PersistentVector
        count: (. v count)
        shift: (. v shift)
        root: (assoc-node (. v shift) (. v root) idx val)
        tail: (. v tail))))))

(defun assoc-node (shift node idx val)
  (if (= shift 0)
    (share (VecNode array: (array-set-copy (. node array) (bit-and idx MASK) val)))
    (let ((subidx (vec-index idx shift)))
      (share (VecNode array:
        (array-set-copy (. node array) subidx
          (assoc-node (- shift BITS) (aget (. node array) subidx) idx val)))))))
```

## Ownership Model

- All nodes `share`d (reference counted)
- `assoc` path-copies root→leaf, shares unchanged siblings
- Tail optimization: most `conj` only copies small tail array

## Tests
```lisp
;; Basic operations
(let ((v (conj (conj (conj (vector) 1) 2) 3)))
  (assert (= (count v) 3))
  (assert (= (nth v 0) 1))
  (assert (= (nth v 2) 3))
  (assert (= (peek v) 3)))

;; Structural sharing
(let ((v1 (conj (conj (vector) 1) 2))
      (v2 (conj v1 3)))
  ;; v1 unchanged
  (assert (= (count v1) 2))
  (assert (= (count v2) 3)))

;; Assoc path copying
(let ((v1 (conj (conj (conj (vector) 1) 2) 3))
      (v2 (assoc v1 1 99)))
  (assert (= (nth v1 1) 2))   ;; original unchanged
  (assert (= (nth v2 1) 99))) ;; new has update

;; Large vector (tests tree structure)
(let ((v (reduce conj (vector) (range 0 1000))))
  (assert (= (count v) 1000))
  (assert (= (nth v 500) 500)))
```

## Acceptance Criteria

- [ ] 32-way trie structure implemented
- [ ] Tail optimization works
- [ ] `nth` O(log32 n) for large vectors
- [ ] `conj`/`assoc` produce new vector, original unchanged
- [ ] Path copying shares unchanged subtrees
- [ ] Ownership checker validates
- [ ] No leaks on repeated operationss

