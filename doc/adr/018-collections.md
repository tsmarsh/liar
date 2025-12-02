# ADR 018: Collections — Persistent vs Conventional

## Status

Accepted

## Context

We need collection types (vectors, maps, lists). There's a fundamental tradeoff:

- **Persistent data structures**: Immutable, structural sharing, O(log n) updates, safe to share
- **Conventional data structures**: Mutable, O(1) access/update, cache-friendly, not safe to share

We want the safe thing to be easy, the fast thing to be explicit.

## Decision

### Syntax

| Syntax | Type | Characteristics |
|--------|------|-----------------|
| `[1 2 3]` | persistent vector | O(log n), structural sharing |
| `{:a 1 :b 2}` | persistent map | O(log n), structural sharing |
| `<[1 2 3]>` | conventional vector | O(1), mutable in place |
| `<{:a 1 :b 2}>` | conventional map | O(1), mutable in place |
| `'(1 2 3)` | list | linked, O(1) prepend |

The `<...>` wrapper consistently means "give me the fast/raw version."

### Operations

**Persistent (returns new value, original unchanged):**

```lisp
(let ((a [1 2 3]))
  (let ((b (conj a 4)))     ; returns NEW vector
    (print a)                ; [1 2 3]
    (print b)))              ; [1 2 3 4]

(let ((m {:a 1}))
  (let ((n (assoc m :b 2))) ; returns NEW map
    (print m)                ; {:a 1}
    (print n)))              ; {:a 1 :b 2}
```

**Conventional (mutates in place):**

```lisp
(let ((a <[1 2 3]>))
  (append &a 4)              ; mutates a
  (print a))                 ; [1 2 3 4]

(let ((m <{:a 1}>))
  (assoc! &m :b 2)           ; mutates m
  (print m))                 ; {:a 1 :b 2}
```

### The Seq Abstraction

All collections support a unified sequence abstraction:

```lisp
(map f [1 2 3])          ; persistent vector
(map f <[1 2 3]>)        ; conventional vector  
(map f '(1 2 3))         ; list
(map f {:a 1 :b 2})      ; map (iterates entries)
(map f "hello")          ; string (iterates chars)
(map f <<1 2 3 4>>)      ; SIMD (iterates elements)
```

Functions that work on sequences work on all collection types:

- `map`, `filter`, `reduce`, `take`, `drop`
- `first`, `rest`, `cons`
- `count`, `empty?`
- `into` (convert between types)

### Threading and Color

Collections have a "color" that affects what context they can be used in:

| Type | Color | Safe to share? | In `plet`? |
|------|-------|----------------|------------|
| Persistent `[...]`, `{...}` | safe | yes | OK |
| Conventional `<[...]>`, `<{...}>` | unsafe | no | ERROR — wrap in atom |
| `(atom <[...]>)` | atomic | yes | OK |

**Persistent in plet — OK:**

```lisp
(plet ((v [1 2 3]))
  (fn (x) (conj v x)))       ; OK: returns new value, no mutation
```

**Conventional in plet — ERROR:**

```lisp
(plet ((v <[1 2 3]>))
  (fn (x) (append &v x)))    ; ERROR: conventional needs atom
```

**Conventional wrapped in atom — OK:**

```lisp
(plet ((v (atom <[1 2 3]>)))
  (fn (x) 
    (swap! v (fn (inner) 
               (append &inner x)
               inner))))      ; OK: atomic access
```

### Return Value Semantics

This is the key difference:

| Operation | Returns | Original |
|-----------|---------|----------|
| `(conj [1 2 3] 4)` | new pointer | unchanged |
| `(append &<[1 2 3]> 4)` | same pointer | mutated |

Persistent operations are pure functions. Conventional operations are mutations.

### Performance Guidance

| Use case | Recommendation |
|----------|----------------|
| Default, general programming | Persistent `[...]` `{...}` |
| Hot loops, tight performance | Conventional `<[...]>` `<{...}>` |
| Shared mutable state | `(atom <[...]>)` |
| Lock-free concurrency | Persistent + atoms |

## Consequences

### Positive

- **Safe by default**: Persistent structures prevent accidental mutation
- **Explicit performance**: `<...>` signals intent and tradeoffs
- **Unified abstraction**: seq works across all collection types
- **Thread safety**: Color system prevents data races at compile time

### Negative

- **Two syntaxes**: Must learn both forms
- **Persistent overhead**: O(log n) vs O(1) for default case
- **Atom ceremony**: Conventional in threads requires wrapping

### Neutral

- Similar to Clojure's model but with explicit conventional option
- `<...>` wrapper is consistent across SIMD and collections
