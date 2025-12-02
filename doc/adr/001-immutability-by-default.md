# ADR 001: Immutability by Default

## Status

Accepted

## Context

We are designing a Lisp that aims for Rust-level memory safety without garbage collection. Traditional Lisps use garbage collection to handle the complexity of mutable, shared data structures. Rust uses a borrow checker, but this requires extensive lifetime annotations and restricts common Lisp patterns.

The fundamental tension:
- Borrow checking requires tracking who can mutate what and when
- Lisp idioms assume values can be freely shared
- Reference counting fails with cycles (common in Lisp: closures, circular data)

## Decision

Values in Corsair are **immutable by default**. A value cannot be modified unless explicitly marked as mutable.

```lisp
(let ((x [1 2 3]))
  (append x 4))        ; ERROR: cannot mutate immutable value
```

Mutation requires explicit opt-in via `&` parameter sigil or mutable bindings.

## Consequences

### Positive

- **Safe sharing**: Multiple references to the same immutable value are always safe
- **No borrow checker complexity for common case**: Most Lisp code is functional and "just works"
- **Easier reasoning**: Values don't change unexpectedly
- **Enables optimization**: Compiler can freely deduplicate, cache, parallelize

### Negative

- **Overhead for mutation-heavy code**: Must use explicit mechanisms
- **Different from traditional Lisps**: `set-car!` style mutation is not the default
- **Learning curve**: Programmers must think about when they need mutability

### Neutral

- Similar to Clojure's model, which has proven successful
- Mutation is possible, just explicit
