# ADR 003: Mutable Reference with & Sigil

## Status

Accepted

## Context

With immutability by default (ADR-001), we need a way to opt into mutation. The question is how to signal that a function will mutate its argument.

Options considered:
1. **Separate functions**: `append` vs `append!` — unclear which argument is mutated
2. **Keyword arguments**: `(foo :mutable x)` — verbose
3. **Sigil on parameter**: `(&x)` — concise, visible in signature
4. **Type annotation**: `(x : mut Vec)` — requires type system

## Decision

Use the `&` sigil in function parameter position to indicate a mutable reference.

```lisp
(defun foo (x)      ; x is immutable reference
  (add x 1))        ; OK: reading

(defun foo (x)      ; x is immutable reference
  (append x 1))     ; ERROR: cannot mutate

(defun foo (&x)     ; x is mutable reference
  (append x 1))     ; OK: caller expects mutation
```

The sigil appears in the function definition, making the contract visible.

## Consequences

### Positive

- **Explicit contract**: Function signature shows what will be mutated
- **Caller awareness**: At call site, you know `(foo x)` might mutate if foo takes `&`
- **Compiler enforced**: Cannot mutate without the sigil
- **Concise**: Single character, minimal noise

### Negative

- **Call site ambiguity**: `(foo x)` doesn't show if x will be mutated — must check foo's signature
- **Not standard Lisp**: Traditional Lisps don't have this distinction

### Neutral

- Similar to Rust's `&mut` but less verbose
- The `&` was chosen over `!` (used for return convention in Scheme) to avoid confusion
