# ADR 017: Numeric Type Promotion

## Status

Accepted

## Context

When mixing numeric types in operations, we need consistent rules for which type "wins". This applies to both scalar operations and SIMD vectors.

## Decision

### Promotion Hierarchy

Smaller/less precise promotes to larger/more precise:

```
i8 → i16 → i32 → i64 → f32 → f64
```

### Scalar Operations

```lisp
(+ 5 6.0)           ; => 11.0 (i64 promoted to f64)
(+ (i8 1) (i32 2))  ; => (i32 3) (i8 promoted to i32)
(* 3 2.5)           ; => 7.5 (i64 promoted to f64)
```

### SIMD Operations

Same rules apply element-wise:

```lisp
(+ <<1 2 3>> <<1.0 2.0 3.0>>)     ; => <<2.0 4.0 6.0>>
(+ <i32<1 2 3>> <<4 5 6>>)        ; => <<5 7 9>> (result is i64)
(* <f32<1.0 2.0>> <<3.0 4.0>>)    ; => <<3.0 8.0>> (result is f64)
```

### What Does NOT Promote

| Operation | Result |
|-----------|--------|
| `(+ 5 "6")` | ERROR: string is not numeric |
| `(+ 5 \A)` | ERROR: char is not numeric |
| `(+ 5 true)` | ERROR: boolean is not numeric |
| `(+ 5 :foo)` | ERROR: keyword is not numeric |

Characters are explicitly not numeric, even though they have code points. Use explicit conversion if needed:

```lisp
(+ 5 (char-code \A))   ; => 70 (explicit conversion OK)
```

### SIMD Length Rules

Type promotion is orthogonal to length matching. Length mismatch is always an error:

```lisp
(+ <<1 2 3>> <<4 5>>)   ; ERROR: length mismatch (not promotable)
```

## Consequences

### Positive

- **Consistent behavior**: Same rules for scalars and vectors
- **Intuitive**: Matches mathematical expectations
- **Safe**: Non-numeric types rejected clearly

### Negative

- **Implicit widening**: Could hide precision issues
- **Performance**: Promotion may require conversion instructions

### Neutral

- Similar to most languages (C, Java, Python)
- Explicit types available when promotion is undesirable:
  ```lisp
  (+ (i32 5) (i32 6))   ; stays i32, no promotion
  ```
