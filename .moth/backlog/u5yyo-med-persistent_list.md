# PersistentList - Immutable Linked List

**Priority:** high
**Category:** lib/collections
**Dependencies:** struct instantiation, closures, trqhk (br/phi)

## Blocked By

- **trqhk** - Recursion requires proper `if` with br/phi
- **regif** - Needs cons cell runtime (which itself needs struct allocation)

## Summary

Implement a persistent (immutable, structurally-shared) linked list in pure liar. Simplest persistent collection - validates ownership/arc on recursive structures before tackling vector and map.

## Data Structure
```lisp
(defstruct Cons (head tail))
;; Empty list is nil
```

## API
```lisp
;; Construction - O(1)
(cons head tail)

;; Access - O(1)
(first lst)           ;; head, nil if empty
(rest lst)            ;; tail, nil if empty

;; Access - O(n)
(nth lst n)

;; Predicates - O(1)
(empty? lst)

;; Operations - O(n)
(count lst)
(reverse lst)
(concat a b)

;; Higher-order - O(n)
(map f lst)
(filter pred lst)
(reduce f init lst)
```

## Implementation

**Location:** `lib/collections/list.liar`
```lisp
(defstruct Cons (head tail))

(defun cons (head tail)
  (share (Cons head: head tail: tail)))

(defun first (lst)
  (if (nil? lst) nil (. lst head)))

(defun rest (lst)
  (if (nil? lst) nil (. lst tail)))

(defun nth (lst n)
  (if (nil? lst)
    nil
    (if (= n 0)
      (first lst)
      (nth (rest lst) (- n 1)))))

(defun empty? (lst) (nil? lst))

(defun count (lst)
  (if (nil? lst) 0 (+ 1 (count (rest lst)))))

(defun reverse (lst)
  (reverse-acc lst nil))

(defun reverse-acc (lst acc)
  (if (nil? lst)
    acc
    (reverse-acc (rest lst) (cons (first lst) acc))))

(defun concat (a b)
  (if (nil? a)
    b
    (cons (first a) (concat (rest a) b))))

(defun map (f lst)
  (if (nil? lst)
    nil
    (cons (f (first lst)) (map f (rest lst)))))

(defun filter (pred lst)
  (if (nil? lst)
    nil
    (if (pred (first lst))
      (cons (first lst) (filter pred (rest lst)))
      (filter pred (rest lst)))))

(defun reduce (f init lst)
  (if (nil? lst)
    init
    (reduce f (f init (first lst)) (rest lst))))
```

## Ownership Model

- `cons` uses `share` for heap allocation with reference counting
- `rest` returns shared reference to tail (no copy)
- Structural sharing automatic: `(cons 0 existing)` shares `existing`

## Tests
```lisp
;; Structural sharing
(let ((tail (cons 2 (cons 3 nil)))
      (lst (cons 1 tail)))
  (assert (= (rest lst) tail)))  ;; same reference

;; Operations
(assert (= (count (cons 1 (cons 2 (cons 3 nil)))) 3))
(assert (= (nth (cons 1 (cons 2 (cons 3 nil))) 1) 2))
(assert (= (reduce + 0 (cons 1 (cons 2 (cons 3 nil)))) 6))

;; Higher-order with closures
(let ((doubled (map (fn (x) (* x 2)) (cons 1 (cons 2 nil)))))
  (assert (= (first doubled) 2))
  (assert (= (first (rest doubled)) 4)))
```

## Acceptance Criteria

- [ ] `Cons` struct, `cons`/`first`/`rest` work
- [ ] `map`/`filter`/`reduce` work with closures
- [ ] Structural sharing verified
- [ ] Ownership checker accepts code
- [ ] No memory leaks (rc cleanup)