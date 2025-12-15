# Protocol Improvements: Clojure Comparison

Compare liar's protocols to Clojure's battle-tested abstractions and identify gaps.

**Priority:** HIGH (foundational for stdlib and liarliar)

## Current liar Protocols

From `lib/liar.seq.liar`:

| Protocol | Methods | Notes |
|----------|---------|-------|
| Seq | `first`, `rest` | Core sequence access |
| Countable | `count` | Element count |
| Indexable | `nth` | Random access |
| Collection | `conj`, `pop` | Add/remove |
| Mappable | `map` | Transform elements |
| Filterable | `filter` | Select elements |
| Reducible | `reduce` | Fold |
| Traversable | `for-each` | Side effects |
| Searchable | `any`, `all`, `member` | Predicates |
| SeqTransform | `seq-reverse`, `seq-append`, `seq-take`, `seq-drop` | Transformations |

## Clojure Protocols (Key Ones)

| Protocol | Methods | Purpose |
|----------|---------|---------|
| Seqable | `seq` | Convert anything to seq |
| ISeq | `first`, `rest`, `cons` | Sequence access |
| Sequential | (marker) | Ordered collection |
| Counted | `count` | O(1) count |
| Indexed | `nth` | O(log n) access |
| ILookup | `get` | Key-value lookup |
| Associative | `assoc`, `contains?`, `entryAt` | Map-like operations |
| Reversible | `rseq` | Reverse sequence |
| IMeta | `meta` | Get metadata |
| IWithMeta | `with-meta` | Set metadata |
| Named | `name`, `namespace` | For symbols/keywords |
| IFn | `invoke` | Make anything callable |
| IDeref | `deref` | Dereference refs |
| IReduce | `reduce` | Efficient reduction |

## Gap Analysis

### Critical Gaps

#### 1. **Seqable** — Convert to Seq
```lisp
(defprotocol Seqable
  (seq [self]))  ;; returns a Seq or nil
```

Why critical: Clojure's `seq` is the universal entry point. Vectors, maps, sets, strings, arrays all become seqs. Without this, every function needs type-specific code.

```lisp
;; With Seqable, this works on anything:
(defun my-count (coll)
  (let ((s (seq coll)))
    (if (nil? s) 0 (add 1 (my-count (rest s))))))
```

#### 2. **ILookup / Associative** — Key-Value Access
```lisp
(defprotocol Lookup
  (get [self key])
  (get-default [self key default]))

(defprotocol Associative
  (assoc [self key val])
  (dissoc [self key])
  (contains? [self key]))
```

Why critical: Maps and vectors both support key/index lookup. Unifies `hm-get` and `nth`.

#### 3. **Callable** — Function Dispatch Protocol
```lisp
(defprotocol Callable
  (invoke [self args]))
```

Why critical: All function calls dispatch through Callable. Functions implement it; non-callable values error at runtime. This provides:
- Fast, uniform dispatch at call sites
- Clear error messages for non-callable values
- Future extensibility (could extend to maps/keywords if desired)

#### 4. **IMeta / IWithMeta** — Metadata
```lisp
(defprotocol Meta
  (meta [self]))

(defprotocol WithMeta
  (with-meta [self m]))
```

Why critical: Attach source locations, type hints, documentation to AST nodes. Essential for compiler.

```lisp
(with-meta '(+ 1 2) {:line 42 :col 3})
```

#### 5. **Named** — Symbols and Keywords
```lisp
(defprotocol Named
  (name [self])       ;; "foo" from :foo or 'foo
  (namespace [self])) ;; "bar" from :bar/foo
```

Why critical: Compiler needs to work with qualified names. `liar.core/map` vs `map`.

### Important Gaps

#### 6. **Empty** — Empty Collection of Same Type
```lisp
(defprotocol Emptyable
  (empty [self]))  ;; returns empty collection of same type
```

Why important: `(into (empty coll) (transform coll))` preserves collection type.

#### 7. **IDeref** — Dereference
```lisp
(defprotocol Deref
  (deref [self]))
```

Why important: Unifies atoms, delays, futures, vars. The `@` syntax sugar.

#### 8. **Reversible** — Efficient Reverse
```lisp
(defprotocol Reversible
  (rseq [self]))  ;; reverse seq without copying
```

Why important: Vectors can reverse in O(1) by iterating backwards. Current `seq-reverse` is O(n).

### Nice to Have

#### 9. **Sorted** — Marker for Sorted Collections
```lisp
(defprotocol Sorted
  (comparator [self]))
```

#### 10. **Sequential** — Marker for Ordered Collections
```lisp
(defprotocol Sequential)  ;; marker, no methods
```

## Recommended Protocol Hierarchy

```
                    Seqable
                       |
              IPersistentCollection
                       |
         +------+------+------+------+
         |      |      |      |      |
       Seq    Map    Set   Vector  String
         |      |      |      |
         +--+---+--+---+--+---+
            |      |      |
         Lookup  Associative
            |
         Callable (IFn)

    Orthogonal:
    - Meta / WithMeta (attach to anything)
    - Named (symbols, keywords)
    - Deref (refs, atoms)
    - Counted (O(1) count)
    - Indexed (O(log n) nth)
    - Reducible (efficient reduce)
    - Reversible (efficient reverse)
```

## Implementation Plan

### Prerequisite: Boxed Cons

Change `Cons` from `(head: i64 tail: ptr)` to `(head: ptr tail: ptr)` so it can hold any boxed value (integers, floats, symbols, keywords, other cons cells, etc.).

### Protocols to Add

1. **Seqable** — `seq` (convert anything to a sequence)
2. **Lookup** — `get`, `get-default` (unified key/index access)
3. **Associative** — `assoc`, `dissoc`, `contains?` (map-like operations)
4. **Named** — `name`, `namespace` (for symbols/keywords)
5. **Meta / WithMeta** — `meta`, `with-meta` (attach source locations, etc.)
6. **Callable** — `invoke` (protocol for function dispatch; error on non-callable)
7. **Empty** — `empty` (return empty collection of same type)
8. **Deref** — `deref` (unify atoms, delays, futures)
9. **Reversible** — `rseq` (efficient reverse without copying)

### Later (not in scope)
- **Sorted** — for tree-based collections
- **Sequential** — marker protocol
- **Equiv** — structural equality protocol

### Design Notes

**Callable:** Function position always dispatches through Callable. Functions implement it; non-callable values error at runtime. This keeps call sites fast and errors clear. If we later want maps/keywords to be callable, we extend Callable to those types — but it's opt-in, not magic.

**Lookup vs Indexable:** Keep both. `Indexed` (renamed from `Indexable`) is for O(log n) positional access. `Lookup` is for key-based access with default values. Vectors implement both.

## Code Changes

### Rename/Consolidate

| Current | Proposed | Reason |
|---------|----------|--------|
| `Indexable` | `Indexed` | Match Clojure naming |
| `Mappable` | Keep or merge into Seq default | |
| `Filterable` | Keep or merge into Seq default | |
| `SeqTransform` | Split into individual protocols | Too wide |

### Add to Existing

```lisp
;; Extend Collection
(defprotocol Collection
  (conj [self x])
  (pop [self])
  (empty [self]))    ;; ADD

;; Extend Seq
(defprotocol Seq
  (first [self])
  (rest [self])
  (cons [self x]))   ;; ADD (or separate Consable)
```

## Example: Unified `get`

With Lookup protocol:

```lisp
;; Works on maps
(get {:a 1 :b 2} :a)  ;; => 1

;; Works on vectors (by index)
(get [10 20 30] 1)    ;; => 20

;; Works on sets (membership)
(get #{:a :b :c} :b)  ;; => :b

;; With default values
(get {:a 1} :b 0)     ;; => 0
```

## Example: Metadata for Compiler

```lisp
(defun parse-expr (tokens)
  (let ((expr (parse-form tokens))
        (loc {:line (current-line) :col (current-col)}))
    (with-meta expr loc)))

(defun compile-error (expr msg)
  (let ((loc (meta expr)))
    (println (str "Error at line " (get loc :line) ": " msg))))
```

## Test Cases

```lisp
;; Seqable
(seq [1 2 3])         ;; => (1 2 3)
(seq {:a 1})          ;; => ([:a 1])
(seq "abc")           ;; => (\a \b \c)
(seq nil)             ;; => nil
(seq [])              ;; => nil

;; Lookup
(get {:a 1} :a)       ;; => 1
(get {:a 1} :b 0)     ;; => 0
(get [1 2 3] 1)       ;; => 2

;; Associative
(assoc {:a 1} :b 2)   ;; => {:a 1 :b 2}
(dissoc {:a 1 :b 2} :a)  ;; => {:b 2}
(contains? {:a 1} :a) ;; => true

;; Named
(name :foo)           ;; => "foo"
(name 'bar)           ;; => "bar"
(namespace :ns/key)   ;; => "ns"

;; Meta
(meta (with-meta '(+ 1 2) {:line 1}))  ;; => {:line 1}

;; Empty
(empty [1 2 3])       ;; => []
(empty {:a 1})        ;; => {}

;; Deref
(deref (atom 42))     ;; => 42

;; Reversible
(rseq [1 2 3])        ;; => (3 2 1)  ;; O(1) for vectors
```

## Related ADRs

- ADR 024: Type-Directed Arithmetic Codegen — Protocol dispatch optimization
- ADR 025: Explicit Arithmetic Names — Standard macros pattern

## Ordering

This moth should be tackled BEFORE the stdlib data transformation moth, as those functions should be built on these protocols.
