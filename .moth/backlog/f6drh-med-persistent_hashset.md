# PersistentHashSet - HAMT-backed Set

**Priority:** med
**Category:** lib/collections
**Dependencies:** PersistentHashMap

## Summary

Persistent set using HAMT. Thin wrapper over PersistentHashMap where values are ignored (or the key itself).

## Data Structure
```lisp
(defstruct PersistentHashSet (impl))  ;; wraps PersistentHashMap
```

## API
```lisp
;; Construction
(hash-set)              ;; empty
(hash-set a b c)        ;; from elements
(set coll)              ;; from collection

;; Query - O(log32 n)
(contains? s val)

;; Update - O(log32 n)
(conj s val)            ;; add
(disj s val)            ;; remove

;; Info - O(1)
(count s)
(empty? s)

;; Set operations - O(n)
(union s1 s2)
(intersection s1 s2)
(difference s1 s2)

;; Iteration
(seq s)                 ;; as list
```

## Implementation

**Location:** `lib/collections/hashset.liar`
```lisp
(defstruct PersistentHashSet (impl))

(def EMPTY-SET (PersistentHashSet impl: (hash-map)))

(defun hash-set () EMPTY-SET)

(defun contains? (s val)
  (not (nil? (get (. s impl) val))))

(defun conj (s val)
  (share (PersistentHashSet impl: (assoc (. s impl) val val))))

(defun disj (s val)
  (share (PersistentHashSet impl: (dissoc (. s impl) val))))

(defun count (s)
  (count (. s impl)))

(defun empty? (s)
  (empty? (. s impl)))

(defun seq (s)
  (keys (. s impl)))

(defun union (s1 s2)
  (reduce conj s1 (seq s2)))

(defun intersection (s1 s2)
  (filter (fn (x) (contains? s2 x)) (seq s1)))

(defun difference (s1 s2)
  (reduce disj s1 (seq s2)))
```

## Tests
```lisp
(let ((s (conj (conj (hash-set) 1) 2)))
  (assert (contains? s 1))
  (assert (contains? s 2))
  (assert (not (contains? s 3)))
  (assert (= (count s) 2)))

;; Idempotent
(let ((s (conj (conj (hash-set) 1) 1)))
  (assert (= (count s) 1)))

;; Set operations
(let ((s1 (conj (conj (hash-set) 1) 2))
      (s2 (conj (conj (hash-set) 2) 3)))
  (assert (= (count (union s1 s2)) 3))
  (assert (= (count (intersection s1 s2)) 1))
  (assert (contains? (intersection s1 s2) 2)))
```

## Acceptance Criteria

- [ ] Wraps PersistentHashMap
- [ ] `conj`/`disj`/`contains?` work
- [ ] Set operations: union, intersection, difference
- [ ] Idempotent add (adding twice = adding once)
- [ ] Ownership checker validates
