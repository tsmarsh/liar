# ADR 007: Aliasing Allowed

## Status

Accepted

## Context

Rust's borrow checker prevents aliasing of mutable references to avoid data races:

```rust
let mut x = vec![1, 2, 3];
let a = &mut x;
let b = &mut x;  // ERROR: cannot have two mutable references
```

This is essential for Rust's memory safety in concurrent code. But it adds complexity.

## Decision

**Aliasing is allowed in Corsair.** The same value can be passed to multiple mutable parameters.

```lisp
(defun bar (&a &b)
  (append a 1)
  (append b 2))

(let ((x [3, 4]))
  (bar x x))             ; OK: x becomes [3, 4, 1, 2]
```

This is safe because:
1. Corsair is single-threaded by default (no data races)
2. Evaluation order is defined (ADR-008)

## Consequences

### Positive

- **Simpler model**: No exclusive borrow rules to learn
- **Flexible code**: Patterns that would require `RefCell` in Rust just work
- **Familiar**: Matches how most dynamic languages behave

### Negative

- **Requires single-threaded assumption**: See ADR-009 for threading
- **Optimizer constraints**: Cannot assume no-aliasing for aggressive optimization
- **Subtle bugs possible**: Order-dependent behavior when aliasing

### Neutral

- This decision is paired with ADR-009 (`let` vs `plet`): concurrent code uses `plet` with atoms, which don't have aliasing issues
- The programmer opts into aliasing by passing the same value twice
