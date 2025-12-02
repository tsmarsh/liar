# ADR 004: Lexical Scope Ownership

## Status

Accepted

## Context

Memory must be freed at some point. Without garbage collection, we need deterministic rules for when values are deallocated.

Options:
1. **Manual memory management**: Error-prone, not Lisp-like
2. **Reference counting**: Simple but fails with cycles
3. **Borrow checker with lifetimes**: Complex, requires annotations
4. **Lexical scope**: Value freed when its binding goes out of scope

## Decision

**Ownership is determined by lexical scope.** A value lives until its binding's scope ends.

```lisp
(let ((x (cons 1 2)))    ; x born, owns cons cell
  (foo x)                 ; x seen by foo (not moved)
  (bar x)                 ; x seen by bar (not moved)
  x)                      ; x dies here, cons cell freed
```

Return values are owned by whoever binds them:

```lisp
(let ((x 5))
  (let ((z (foo x)))     ; z owns whatever foo returns
    (add x z)))          ; x still valid
                         ; z freed here
                         ; x freed here
```

## Consequences

### Positive

- **Simple mental model**: Values live in their `let` block
- **Deterministic**: No GC pauses, predictable deallocation
- **No annotations**: Lifetimes are implicit from code structure
- **Composable**: Nested scopes nest lifetimes naturally

### Negative

- **Returned values need thought**: Functions return fresh values, not references to locals
- **Long-lived values**: Must be bound at appropriate scope level

### Neutral

- Combined with ADR-005 (closure captures), closures can extend lifetime by taking ownership
- This is simpler than Rust because we pass by reference (ADR-002), not by move
