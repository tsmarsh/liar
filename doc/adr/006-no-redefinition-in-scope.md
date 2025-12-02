# ADR 006: No Redefinition in Same Scope

## Status

Accepted

## Context

Many Lisps and languages allow shadowing variables in nested scopes:

```lisp
(let ((x 5))
  (let ((x 6))    ; shadows outer x
    x))           ; => 6
```

This can lead to confusion about which `x` is being referenced, especially with mutation.

## Decision

**No redefinition of a variable in the same or enclosing scope.**

```lisp
(let ((x 5)
      (x 6))             ; ERROR: x already bound in this let
  ...)

(let ((x 5))
  (let ((x 6))           ; ERROR: x exists in enclosing scope
    ...))
```

**Shadowing across function boundaries is allowed:**

```lisp
(defun foo (y)
  (let ((x (inc y)))     ; this x is local to foo
    x))

(let ((x 5))
  (foo x))               ; foo's x doesn't conflict with this x
```

The function boundary creates a new namespace.

## Consequences

### Positive

- **Clarity**: Every reference to `x` in a scope refers to the same binding
- **Fewer bugs**: Can't accidentally shadow and then wonder which x you're mutating
- **Easier refactoring**: Renaming is straightforward

### Negative

- **Less flexible**: Must choose unique names within a scope
- **Different from Scheme/Racket**: Those allow arbitrary shadowing
- **Longer names sometimes**: Can't reuse short names

### Neutral

- Function parameters create their own scope, so `(defun foo (x) ...)` doesn't conflict
- This matches some typed languages (certain ML dialects)
