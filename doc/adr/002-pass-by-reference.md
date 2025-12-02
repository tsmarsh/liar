# ADR 002: Pass by Reference for Compound Types

## Status

Accepted

## Context

When passing values to functions, we have several options:
1. **Pass by value (copy)**: Safe but expensive for large structures
2. **Pass by reference**: Efficient but raises questions about ownership
3. **Move semantics**: Efficient but restricts usage patterns

Rust uses move semantics by default, requiring explicit borrowing or cloning. This is safe but creates friction and requires lifetime annotations.

## Decision

Corsair uses **pass by reference** for compound types (cons cells, vectors, maps, closures) and **pass by value (copy)** for primitives (numbers, booleans).

```lisp
(defun foo (x)    ; x is an immutable reference to caller's data
  (car x))        ; reading is allowed

(let ((data [1 2 3]))
  (foo data)      ; data passed by reference
  (len data))     ; data still accessible — not moved
```

Functions receive a reference to the caller's data, not ownership. The caller retains ownership.

## Consequences

### Positive

- **No "use after move" errors**: Values aren't consumed by function calls
- **No lifetime annotations**: References are valid for the function call duration
- **Efficient**: No copying of large structures
- **Familiar**: Similar to Java's object references, Python's model

### Negative

- **Must combine with immutability**: If references could mutate freely, we'd have aliasing bugs
- **Different from Rust**: Rust programmers may expect move semantics

### Neutral

- Combined with ADR-001 (immutability), this is safe
- Combined with ADR-003 (& sigil), mutation is possible but explicit
