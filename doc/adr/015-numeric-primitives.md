# ADR 015: Numeric Primitives

## Status

Accepted

## Context

We need to define the numeric primitive types, their default sizes, and how to handle overflow.

## Decision

### Integer Types

| Syntax | Type |
|--------|------|
| `42` | i64 (default) |
| `(i8 42)` | i8 |
| `(i16 42)` | i16 |
| `(i32 42)` | i32 |
| `(i64 42)` | i64 |

### Float Types

| Syntax | Type |
|--------|------|
| `3.14` | f64 (default) |
| `(f32 3.14)` | f32 |
| `(f64 3.14)` | f64 |

### Overflow Semantics

| Context | Behavior |
|---------|----------|
| Default | Error on overflow |
| `(boxed expr)` | Promotes to biginteger, never overflows |
| `(wrapping expr)` | C/Rust-style silent wrap |

```lisp
(* LARGE_INT LARGER_INT)           ; ERROR: overflow
(boxed (* LARGE_INT LARGER_INT))   ; => (biginteger ...)
(wrapping (* 255 2))               ; => 254 (for i8)
```

### Other Primitives

| Syntax | Type |
|--------|------|
| `true` / `false` | boolean |
| `"hello"` | string (immutable, UTF-8) |
| `\a` | character |
| `'foo` | symbol (interned) |
| `:foo` | keyword |
| `#[0x48 0x65]` | byte array |

### Reader Macros

| Syntax | Type |
|--------|------|
| `#r"pattern"` | regex |

## Consequences

### Positive

- **Safe by default**: Overflow errors catch bugs early
- **Explicit when needed**: `boxed` for arbitrary precision, `wrapping` for performance
- **Familiar syntax**: Literals look like other Lisps
- **Clear sizing**: Explicit type constructors when size matters

### Negative

- **Overflow errors may surprise**: C/Rust programmers expect wrapping
- **Performance cost**: Default overflow checking has runtime cost

### Neutral

- Default i64/f64 matches modern 64-bit systems
- `boxed` is a macro, not a type — promotes based on value
