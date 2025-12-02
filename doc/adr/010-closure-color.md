# ADR 010: Closure Color Tracking

## Status

Accepted

## Context

With `let` vs `plet` (ADR-009), closures have different threading capabilities. A closure from `let` cannot be passed to `pmap`. But what if a function returns a closure?

```lisp
(defun make-counter ()
  (let ((n 0))
    (fn () (set! n (inc n)) n)))

(pmap (make-counter) [1 2 3])   ; Is this safe?
```

The compiler must track that `make-counter` returns a `let`-closure.

## Decision

Closures have a **"color"** tracked by the compiler:

| Color | Created by | Can pass to |
|-------|------------|-------------|
| `let`-closure | `let` with mutable captures | `map`, `filter`, single-threaded ops |
| `plet`-closure | `plet` (atoms + constants) | `map`, `pmap`, `pfilter`, any |
| pure | no mutable captures | anywhere |

The color flows through function calls:

```lisp
(defun make-unsafe ()
  (let ((n 0))
    (fn () (set! n (inc n)) n)))  ; returns let-closure

(defun make-safe ()
  (plet ((n (atom 0)))
    (fn () (swap! n inc) @n)))    ; returns plet-closure

(defun make-pure ()
  (fn (x) (* x 2)))               ; returns pure closure

(pmap (make-unsafe) [1 2 3])      ; ERROR: let-closure
(pmap (make-safe) [1 2 3])        ; OK: plet-closure  
(pmap (make-pure) [1 2 3])        ; OK: pure
```

The compiler infers return type includes closure color.

## Consequences

### Positive

- **Safety through composition**: Can't accidentally pass unsafe closure to pmap
- **No runtime checks**: Compiler tracks colors statically
- **Clear error messages**: "let-closure passed to parallel operation"

### Negative

- **Type system complexity**: Closure types now have a color component
- **Inference required**: Compiler must track colors through all code paths
- **API constraints**: Library functions must be colored appropriately

### Neutral

- This is similar to Rust's `Send`/`Sync` traits, but simpler
- Colors are inferred, not annotated
- Pure closures (no mutable captures) are the most flexible
