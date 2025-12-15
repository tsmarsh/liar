# Stdlib: Data Transformation Elegance

Enhance `lib/liar.core.liar` and `lib/liar.seq.liar` with functions that make data manipulation elegant.

**Priority:** HIGH (critical for liarliar self-hosting — compiler is data transformation)

## Related ADRs

- [ADR 025: Explicit Arithmetic Names](../../doc/adr/025-explicit-arithmetic-names.md) — Standard macros pattern

## Why This Matters

With homoiconicity, code IS data. The self-hosted compiler is fundamentally a data transformation pipeline: s-expressions in, s-expressions out. Elegant data manipulation = elegant compiler code.

## Design Principle: Protocol-First

**All functions must be built on protocols, not concrete types.**

Functions should use `first`, `rest`, `map`, `filter`, `reduce`, `conj`, `nth`, `count` — the protocol methods — not Cons-specific field access. This ensures:

1. Functions work with any Seq implementation (lists, vectors, lazy seqs)
2. New collection types get all functions "for free" by implementing protocols
3. Compiler can optimize based on protocol dispatch

```lisp
;; GOOD - uses protocols
(defun second (seq) (first (rest seq)))

;; BAD - hardcoded to Cons
(defun second (seq) (. (. seq tail) head))
```

## Threading Macros (Critical)

Transform nested calls into readable pipelines:

```lisp
;; Define in lib/liar.core.liar

;; Thread-first: insert x as first argument
;; (-> x (f a) (g b)) => (g (f x a) b)
(defmacro -> (x ... forms)
  (reduce forms
    (fn (acc form)
      (if (cons? form)
          (cons (first form) (cons acc (rest form)))
          (list2 form acc)))
    x))

;; Thread-last: insert x as last argument
;; (->> x (f a) (g b)) => (g a (f b x))
(defmacro ->> (x ... forms)
  (reduce forms
    (fn (acc form)
      (if (cons? form)
          (append form (list1 acc))
          (list2 form acc)))
    x))

;; Thread with placeholder
;; (as-> x $ (f $ a) (g b $)) => (g b (f x a))
(defmacro as-> (x name ... forms)
  `(let ((,name ,x))
     ,@(map forms (fn (form) `(let ((,name ,form)) ,name)))))
```

**Usage:**
```lisp
;; Before
(filter (map (reverse data) transform) valid?)

;; After
(-> data
    reverse
    (map transform)
    (filter valid?))
```

## Sequence Functions (Protocol-Based)

All functions use protocol methods (`first`, `rest`, `map`, `filter`, `reduce`, `conj`, `nth`, `count`):

```lisp
;; Quick accessors — use Seq protocol
(defun second (seq) (first (rest seq)))
(defun third (seq) (first (rest (rest seq))))
(defun fourth (seq) (first (rest (rest (rest seq)))))
(defun last (seq) (nth seq (sub (count seq) 1)))  ;; uses Indexable + Countable

;; Find first element matching predicate (returns element, not bool)
;; Uses Seq protocol (first, rest)
(defun find (pred seq)
  (if (nil? seq)
      nil
      (if (pred (first seq))
          (first seq)
          (find pred (rest seq)))))

;; some - find first truthy result of (f elem)
(defun some (f seq)
  (if (nil? seq)
      nil
      (let ((result (f (first seq))))
        (if result result (some f (rest seq))))))

;; mapcat - map then concatenate results
(defun mapcat (f seq)
  (if (nil? seq)
      nil
      (append (f (first seq)) (mapcat f (rest seq)))))

;; flatten - flatten nested lists one level
(defun flatten (seq)
  (mapcat (fn (x) (if (cons? x) x (list1 x))) seq))

;; flatten-all - fully flatten nested structure
(defun flatten-all (seq)
  (if (nil? seq)
      nil
      (if (cons? (first seq))
          (append (flatten-all (first seq)) (flatten-all (rest seq)))
          (cons (first seq) (flatten-all (rest seq))))))

;; interpose - insert separator between elements
(defun interpose (sep seq)
  (if (nil? seq)
      nil
      (if (nil? (rest seq))
          seq
          (cons (first seq) (cons sep (interpose sep (rest seq)))))))

;; zip - combine two sequences pairwise
(defun zip (seq1 seq2)
  (if (or (nil? seq1) (nil? seq2))
      nil
      (cons (list2 (first seq1) (first seq2))
            (zip (rest seq1) (rest seq2)))))

;; zip-with - combine with function
(defun zip-with (f seq1 seq2)
  (if (or (nil? seq1) (nil? seq2))
      nil
      (cons (f (first seq1) (first seq2))
            (zip-with f (rest seq1) (rest seq2)))))

;; partition - split into chunks of n
(defun partition (n seq)
  (if (nil? seq)
      nil
      (cons (take n seq) (partition n (skip-n n seq)))))

;; group-by - group elements by key function
(defun group-by (f seq)
  ;; Returns list of (key . elements) pairs
  ;; Implementation needs hashmap or assoc list
  ...)

;; range - generate sequence of integers
(defun range (start end)
  (if (>= start end)
      nil
      (cons start (range (inc start) end))))

;; range-step
(defun range-step (start end step)
  (if (>= start end)
      nil
      (cons start (range-step (add start step) end step))))

;; repeat - n copies of value
(defun repeat-n (n x)
  (if (<= n 0)
      nil
      (cons x (repeat-n (dec n) x))))

;; iterate - generate sequence by repeated application
(defun iterate-n (n f x)
  (if (<= n 0)
      nil
      (cons x (iterate-n (dec n) f (f x)))))

;; concat - concatenate multiple sequences
(defun concat2 (a b) (append a b))
(defun concat3 (a b c) (append a (append b c)))
(defun concat4 (a b c d) (append a (append b (append c d))))
```

## Function Combinators

```lisp
;; partial - partial application (fixed arity versions)
(defun partial1 (f a)
  (fn (x) (f a x)))

(defun partial2 (f a b)
  (fn (x) (f a b x)))

;; juxt - apply multiple functions, return list of results
(defun juxt2 (f g)
  (fn (x) (list2 (f x) (g x))))

(defun juxt3 (f g h)
  (fn (x) (list3 (f x) (g x) (h x))))

;; apply - apply function to list of args (fixed arity)
(defun apply1 (f args)
  (f (first args)))

(defun apply2 (f args)
  (f (first args) (second args)))

(defun apply3 (f args)
  (f (first args) (second args) (third args)))

;; pipe - compose functions left-to-right (opposite of comp)
(defun pipe (f g)
  (fn (x) (g (f x))))

(defun pipe3 (f g h)
  (fn (x) (h (g (f x)))))
```

## Map Operations

For nested data access (useful for AST manipulation):

```lisp
;; Assumes hashmap with hm-get, hm-assoc

;; get-in - nested access
(defun get-in (m keys)
  (if (nil? keys)
      m
      (get-in (hm-get m (first keys)) (rest keys))))

;; assoc-in - nested update
(defun assoc-in (m keys v)
  (if (nil? (rest keys))
      (hm-assoc m (first keys) v)
      (hm-assoc m (first keys)
                (assoc-in (hm-get m (first keys)) (rest keys) v))))

;; update-in - nested update with function
(defun update-in (m keys f)
  (assoc-in m keys (f (get-in m keys))))

;; select-keys - keep only specified keys
(defun select-keys (m keys)
  (reduce keys
    (fn (acc k) (hm-assoc acc k (hm-get m k)))
    (hash-map)))

;; merge - combine maps (later keys win)
(defun merge2 (m1 m2)
  ;; iterate m2 entries, assoc into m1
  ...)
```

## Test Cases

```lisp
;; Threading
(-> (list3 1 2 3) (map square) sum)  ;; => 14

;; Sequence ops
(second (list3 1 2 3))  ;; => 2
(find evenp (list3 1 2 3))  ;; => 2
(mapcat (fn (x) (list2 x x)) (list2 1 2))  ;; => (1 1 2 2)
(interpose 0 (list3 1 2 3))  ;; => (1 0 2 0 3)
(zip (list2 1 2) (list2 'a 'b))  ;; => ((1 a) (2 b))
(partition 2 (range 0 6))  ;; => ((0 1) (2 3) (4 5))
(range 0 5)  ;; => (0 1 2 3 4)

;; Combinators
((juxt2 inc dec) 5)  ;; => (6 4)
((partial1 add 10) 5)  ;; => 15
```

## New Protocols to Consider

Some functions might be better as protocol methods for type-specific optimization:

```lisp
;; Zippable - types that can be zipped
(defprotocol Zippable
  (zip [self other])
  (zip-with [self other f]))

;; Flattenable - types that can be flattened
(defprotocol Flattenable
  (flatten [self]))

;; Partitionable - types that support chunking
(defprotocol Partitionable
  (partition [self n])
  (partition-by [self f]))

;; Associative - types with key-value access (maps, vectors by index)
(defprotocol Associative
  (get [self key])
  (assoc [self key val])
  (dissoc [self key])
  (keys [self])
  (vals [self]))

;; NestedAssociative - nested access
(defprotocol NestedAssociative
  (get-in [self keys])
  (assoc-in [self keys val])
  (update-in [self keys f]))
```

Default implementations can be provided via `extend-protocol-default` for any Seq, just like `Countable` defaults to walking the sequence.

## Ordering

Depends on: `liar.seq` (Cons, protocols), `liar.hashmap`
Required by: `liarliar` (self-hosted compiler)

## Implementation Notes

**Protocol-first:** Every function should be expressible in terms of protocols. If a function needs type-specific access, that's a sign a new protocol is needed.

Threading macros (`->`, `->>`) need variadic macro support or can be implemented with a fixed number of forms initially.

Many functions have recursive implementations shown here. For bootstrap, these are fine. Later optimization could use tail recursion or iterative versions.

The `group-by` and `merge` functions need hashmap iteration which may not exist yet. Can defer or implement with association lists.

**Type-directed optimization:** Per ADR 024, if the compiler knows the concrete type, it can emit direct calls to optimized implementations instead of protocol dispatch.
