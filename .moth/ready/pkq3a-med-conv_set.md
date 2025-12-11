# ConvSet - Mutable Hash Set

**Priority:** low
**Category:** lib/collections
**Dependencies:** ConvMap

## Summary

Mutable hash set wrapping ConvMap. Same pattern as PersistentHashSet wrapping PersistentHashMap.

## Syntax
```lisp
<#{a b c}>          ;; mutable set literal (if we add syntax)
;; or just
(mut-set a b c)
```

## Data Structure
```lisp
(defstruct MutSet (impl))  ;; wraps MutMap
```

## API
```lisp
;; Construction
(mut-set)
(mut-set a b c)

;; Query - O(1) average
(contains?! s val)

;; Mutation - O(1) average
(add! s val)
(remove! s val)

;; Info
(len s)
(empty?! s)

;; Set operations
(union! s1 s2)          ;; mutates s1
(intersect! s1 s2)      ;; mutates s1
(difference! s1 s2)     ;; mutates s1

;; Conversion
(freeze s)              ;; -> PersistentHashSet
```

## Implementation

**Location:** `lib/collections/mut-set.liar`
```lisp
(defstruct MutSet (impl))

(defun mut-set ()
  (MutSet impl: (mut-map)))

(defun contains?! (s val)
  (contains?! (. s impl) val))

(defun add! (s val)
  (put! (. s impl) val val)
  s)

(defun remove! (s val)
  (remove! (. s impl) val)
  s)

(defun len (s) (len (. s impl)))
(defun empty?! (s) (empty?! (. s impl)))

(defun seq! (s) (keys! (. s impl)))

(defun union! (s1 s2)
  (for-each (fn (x) (add! s1 x)) (seq! s2))
  s1)

(defun intersect! (s1 s2)
  (for-each
    (fn (x) (unless (contains?! s2 x) (remove! s1 x)))
    (seq! s1))
  s1)

(defun difference! (s1 s2)
  (for-each (fn (x) (remove! s1 x)) (seq! s2))
  s1)

(defun freeze (s)
  (let ((ps (reduce conj (hash-set) (seq! s))))
    (clear! (. s impl))
    ps))
```

## Tests
```lisp
(let ((s (mut-set)))
  (add! s 1)
  (add! s 2)
  (add! s 1)  ;; duplicate
  (assert (= (len s) 2))
  (assert (contains?! s 1))
  (assert (contains?! s 2))
  (assert (not (contains?! s 3))))

;; Set operations
(let ((s1 (mut-set))
      (s2 (mut-set)))
  (add! s1 1) (add! s1 2)
  (add! s2 2) (add! s2 3)
  (union! s1 s2)
  (assert (= (len s1) 3)))
```

## Acceptance Criteria

- [ ] Wraps MutMap
- [ ] `add!`/`remove!`/`contains?!`
- [ ] Mutating set operations
- [ ] `freeze` to PersistentHashSet
