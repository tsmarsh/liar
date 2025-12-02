# ADR 016: SIMD Vectors

## Status

Accepted

## Context

Modern CPUs have SIMD (Single Instruction Multiple Data) capabilities. We want SIMD to be as natural to use as other primitives, not a special optimization.

There's a naming collision: "vector" could mean SIMD registers or a growable collection. We need distinct syntax.

## Decision

### Syntax

| Syntax | Type | Description |
|--------|------|-------------|
| `<<1 2 3 4>>` | v4 i64 | Inferred element type and count |
| `<<1.0 2.0 3.0 4.0>>` | v4 f64 | Float elements |
| `<i8<1 2 3 4>>` | v4 i8 | Explicit element type |
| `<f32<1.0 2.0 3.0 4.0>>` | v4 f32 | Explicit float type |

### Operations

Standard arithmetic operators work on SIMD vectors:

```lisp
(+ <<1 2 3 4>> <<5 6 7 8>>)      ; => <<6 8 10 12>>
(* <<1.0 2.0>> <<3.0 4.0>>)      ; => <<3.0 8.0>>
```

### Scalar Broadcast

Scalars automatically broadcast:

```lisp
(+ 5 <<1 2 3>>)                   ; => <<6 7 8>>
(* <<1 2 3 4>> 2)                 ; => <<2 4 6 8>>
```

### Length Handling

The programmer specifies element count in the literal. The compiler maps to hardware:

```lisp
; You write (on a 4-wide machine):
(+ 4 <<1 2 3 4 5>>)

; Compiler generates:
; chunk1: <<1 2 3 4>> + <<4 4 4 4>> = <<5 6 7 8>>
; chunk2: <<5>> + <<4>>            = <<9>>
; result: <<5 6 7 8 9>>
```

The programmer never thinks about hardware SIMD width. The compiler handles chunking and cleanup.

### Error Cases

| Operation | Result |
|-----------|--------|
| `(+ <<1 2 3>> <<4 5 6 7>>)` | ERROR: length mismatch |
| `(+ <<1 2 3>> "foo")` | ERROR: non-numeric type |
| `(+ <<1 2 3>> \A)` | ERROR: char is not numeric |

### Type Promotion

Different numeric types are promoted (see ADR-017):

```lisp
(+ <<1 2 3>> <<1.0 2.0 3.0>>)     ; => <<2.0 4.0 6.0>> (i64 → f64)
(+ <i32<1 2 3>> <<4 5 6>>)        ; => <<5 7 9>> (i32 → i64)
```

### Comparison with Collections

| Syntax | What | Size |
|--------|------|------|
| `<<1 2 3 4>>` | SIMD vector | Fixed, compile-time |
| `(vector seq)` | Collection | Dynamic, runtime |
| `[1 2 3]` | Array | Growable |

Both `<<...>>` and `(vector seq)` get vectorized operations. The difference is when length is known.

## Consequences

### Positive

- **First-class SIMD**: As easy as regular arithmetic
- **No width management**: Compiler handles hardware mapping
- **Type safety**: Length and type mismatches caught at compile time
- **Clear syntax**: `<<...>>` visually distinct from collections

### Negative

- **New syntax to learn**: `<<...>>` is non-standard
- **Compile-time vs runtime**: Must choose appropriate construct

### Neutral

- Hardware-specific optimizations are compiler's job
- Explicit element types available when needed

## Open Questions

1. **Width restrictions**: Should we require power-of-2 lengths, or let LLVM handle arbitrary widths?
2. **Alignment**: Do we need syntax for specifying alignment?
