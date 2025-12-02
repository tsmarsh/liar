# ADR 005: Closures Own Captured State

## Status

Accepted

## Context

Closures that capture mutable state are fundamental to Lisp idioms. The classic example:

```lisp
(defun make-counter ()
  (let ((n 0))
    (fn () (set! n (inc n)) n)))
```

The closure must outlive the `let` that created `n`. This creates a lifetime problem.

Options:
1. **Disallow**: Too restrictive, breaks Lisp idioms
2. **Reference counting**: Closure holds ref to n, freed when count hits 0
3. **Move semantics**: Closure takes ownership of n

## Decision

When a closure captures a value and the closure escapes its defining scope (returned, stored), the captured value **moves into the closure**. The closure owns it.

```lisp
(defun accumulate ()   
  (let ((cache []))           ; cache created here
    (fn (x) 
      (append cache x)        ; mutate cache
      (sum cache))))          ; return sum

; cache moves INTO the closure
; closure owns cache now

(let ((acc (accumulate)))     ; acc owns the closure, closure owns cache
  (acc 4)                     ; cache is [4]
  (acc 5))                    ; cache is [4,5]
                              ; acc dies, closure dies, cache dies
```

Ownership chain: `binding` → `closure` → `captured values`

## Consequences

### Positive

- **Classic patterns work**: Counters, accumulators, encapsulated state
- **Clear ownership**: Closure owns its environment
- **No reference counting needed**: Single owner (the closure)
- **Deterministic cleanup**: When closure dies, captures die

### Negative

- **Original binding is "consumed"**: Can't access `cache` after defining the closure that captures it mutably
- **Must return the closure**: If closure isn't returned/stored, capture doesn't make sense

### Neutral

- This is similar to Rust's `move` closures, but inferred rather than explicit
- Read-only captures of immutable values don't need to move (can share)
