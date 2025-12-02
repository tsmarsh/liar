# ADR 008: List Order is Evaluation Order

## Status

Accepted

## Context

With aliasing allowed (ADR-007) and mutable state, evaluation order matters:

```lisp
(let ((x []))
  (foo (bar x) (baz x)))  ; does bar or baz run first?
```

If `bar` and `baz` both mutate `x`, the result depends on order.

Some languages leave evaluation order undefined (C), some define it strictly (most).

## Decision

**Evaluation order follows list order (left-to-right).**

```lisp
(let ((x [3, 4]))
  (let ((y (foo x))      ; first: foo runs
        (z (foo x)))     ; second: foo runs again
    ...))

(f (g x) (h x))          ; g runs, then h runs, then f
```

This applies to:
- `let` bindings (top to bottom)
- Function arguments (left to right)
- List elements

## Consequences

### Positive

- **Predictable**: No surprises about mutation order
- **Debuggable**: Mental execution matches actual execution
- **Aliasing is safe**: Combined with ADR-007, order-dependent behavior is defined

### Negative

- **Less optimization freedom**: Compiler cannot reorder independent computations
- **Sequential by default**: Must use explicit parallelism for concurrent execution

### Neutral

- This is standard for most Lisps and dynamic languages
- Parallel operations (`pmap`) explicitly opt out of this ordering
