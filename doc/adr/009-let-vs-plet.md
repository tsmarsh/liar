# ADR 009: let vs plet for Thread Safety

## Status

Accepted

## Context

Corsair allows mutable state in closures (ADR-005). This is not thread-safe:

```lisp
(defun accumulate ()
  (let ((cache []))
    (fn (x) (append cache x) (sum cache))))

(let ((acc (accumulate)))
  (pmap acc [1 2 3]))     ; DANGER: multiple threads mutate cache
```

We need a way to:
1. Allow mutable closures for single-threaded code
2. Require thread-safe constructs for parallel code
3. Make the compiler enforce this

## Decision

Two binding forms with different threading guarantees:

| Binding | Mutable captures | Thread safety | Use with |
|---------|------------------|---------------|----------|
| `let` | regular values | NOT thread-safe | `map`, `filter`, single-threaded ops |
| `plet` | atoms only | thread-safe | `map`, `pmap`, `pfilter`, any |

```lisp
; Single-threaded — let is fine
(defun accumulate ()
  (let ((cache []))
    (fn (x) (append cache x) (sum cache))))

(map (accumulate) [1 2 3])     ; OK
(pmap (accumulate) [1 2 3])    ; ERROR: let-closure to parallel op

; Thread-safe — plet with atoms
(defun accumulate ()
  (plet ((cache (atom [])))
    (fn (x) 
      (swap! cache (fn (c) (append c x)))
      (sum @cache))))

(pmap (accumulate) [1 2 3])    ; OK
```

**plet rules:**
- Mutable bindings must be `atom`
- Non-atom bindings are constants (immutable)
- Compiler enforces: cannot mutate non-atom in plet

```lisp
(plet ((cache (atom []))    ; mutable — atom required
       (multiplier 10))     ; immutable — constant, no atom needed
  (fn (x) 
    (swap! cache ...)       ; OK: atom
    (set! multiplier 20)))  ; ERROR: not an atom
```

## Consequences

### Positive

- **Compile-time safety**: Threading bugs caught at compile time
- **Explicit intent**: `plet` signals "this will be used concurrently"
- **Zero runtime overhead**: No locks for single-threaded `let` code
- **Gradual adoption**: Start with `let`, upgrade to `plet` when needed

### Negative

- **Two constructs to learn**: Must understand when to use which
- **Migration cost**: Moving from single to multi-threaded requires changing `let` to `plet`

### Neutral

- Constants in `plet` don't need `atom` — only mutable state does
- `plet`-closures work with both `map` and `pmap`; `let`-closures only with `map`
