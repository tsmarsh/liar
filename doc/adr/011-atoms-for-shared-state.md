# ADR 011: Atoms for Thread-safe Mutable State

## Status

Accepted

## Context

In `plet` closures, mutable state must be thread-safe. We need a primitive for atomic mutable state.

Options:
1. **Locks/mutexes**: Explicit locking, error-prone (deadlocks)
2. **STM (Software Transactional Memory)**: Powerful but complex
3. **Atoms (atomic CAS)**: Simple, lock-free, composable

## Decision

Use **atoms** for thread-safe mutable state, following Clojure's model.

```lisp
(plet ((counter (atom 0)))      ; create atomic cell
  (fn ()
    (swap! counter inc)          ; atomic update
    @counter))                   ; atomic read
```

**Operations:**

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Create | `(atom value)` | Create atomic cell with initial value |
| Read | `@atom` or `(deref atom)` | Get current value |
| Update | `(swap! atom fn)` | Apply fn to current value, set result |
| Set | `(reset! atom value)` | Set to new value |
| CAS | `(compare-and-set! atom old new)` | Set if current == old |

**swap! semantics:**

```lisp
(swap! counter (fn (current) (+ current 1)))
```

The function is applied atomically. If another thread modified the atom, `swap!` retries with the new value.

## Consequences

### Positive

- **Lock-free**: No deadlocks, good performance
- **Simple API**: Just `swap!` and `@`
- **Composable**: Functions passed to swap! are pure
- **Proven model**: Clojure has used this successfully for 15+ years

### Negative

- **Not suitable for coordinated updates**: Two atoms can't be updated atomically together (see ADR-012)
- **Retry overhead**: High contention can cause many retries
- **No blocking**: Can't wait for a condition (use other primitives)

### Neutral

- Atoms are for independent mutable values
- For coordinated multi-value updates, see ADR-012 (dosync)
