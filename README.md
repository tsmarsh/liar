# lIR

An S-expression assembler for LLVM IR. Not a Lisp—just LLVM IR in parens.

## What is this?

lIR (pronounced "liar") provides a 1:1 mapping to LLVM IR using S-expression syntax. There's no evaluation model, no macros, no type promotion—just LLVM IR with parentheses.

```lisp
(fadd (double 5.0) (double 6.0))
```

Maps directly to:

```llvm
%0 = fadd double 5.0, 6.0
```

## Why?

lIR is a foundation layer for building higher-level languages. It provides:

- **Exact LLVM IR semantics** — no surprises
- **No implicit conversions** — types must match exactly
- **No operator overloading** — `add` for integers, `fadd` for floats
- **Predictable behavior** — what you write is what you get

## Quick Reference

### Types

```lisp
; Integers
(i1 1)              ; boolean (true)
(i8 42)             ; 8-bit
(i32 -1)            ; 32-bit
(i64 9223372036854775807)

; Floats
(float 3.14)
(double 2.718281828)
(double inf)        ; infinity
(double nan)        ; not a number

; Vectors
(<4 x i32> 1 2 3 4)
(<2 x double> 1.0 2.0)
```

### Arithmetic

```lisp
; Integer (operands must match)
(add (i32 5) (i32 3))       ; => (i32 8)
(sub (i32 10) (i32 3))      ; => (i32 7)
(mul (i32 6) (i32 7))       ; => (i32 42)
(sdiv (i32 -10) (i32 3))    ; => (i32 -3) signed
(udiv (i32 -1) (i32 2))     ; => (i32 2147483647) unsigned

; Float
(fadd (double 1.5) (double 2.5))  ; => (double 4.0)
(fmul (double 2.5) (double 4.0))  ; => (double 10.0)
```

### Comparisons

```lisp
; Integer comparison (returns i1)
(icmp eq (i32 5) (i32 5))   ; => (i1 1)
(icmp slt (i32 -1) (i32 1)) ; => (i1 1) signed less than
(icmp ult (i32 -1) (i32 1)) ; => (i1 0) unsigned (-1 is MAX)

; Float comparison
(fcmp olt (double 1.0) (double 2.0))  ; => (i1 1)
(fcmp uno (double nan) (double 1.0))  ; => (i1 1) unordered (NaN)
```

### Conversions

```lisp
; Integer size changes
(trunc i8 (i32 257))        ; => (i8 1)
(zext i32 (i8 255))         ; => (i32 255) zero-extend
(sext i32 (i8 -1))          ; => (i32 -1) sign-extend

; Int/float conversions
(sitofp double (i32 -42))   ; => (double -42.0)
(fptosi i32 (double 3.7))   ; => (i32 3)
```

### Control Flow

```lisp
; Select (ternary)
(select (icmp slt (i32 5) (i32 10)) (i32 1) (i32 2))  ; => (i32 1)
```

### Vectors

```lisp
(extractelement (<4 x i32> 10 20 30 40) (i32 0))  ; => (i32 10)
(insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 0))  ; => (<4 x i32> 99 2 3 4)
(shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 5 6 7 8) (<4 x i32> 0 4 1 5))
; => (<4 x i32> 1 5 2 6)
```

## Type Safety

lIR has strict type checking. No implicit conversions:

```lisp
(add (i8 1) (i32 2))              ; ERROR: type mismatch
(add (sext i32 (i8 1)) (i32 2))   ; OK: explicit conversion

(add (double 1.0) (double 2.0))   ; ERROR: use fadd for floats
(fadd (double 1.0) (double 2.0))  ; OK
```

## Documentation

- [Language Guide](doc/LANGUAGE.md) — comprehensive reference

## Project Structure

```
lir/
├── README.md
├── CLAUDE.md           # AI assistant instructions
├── doc/
│   └── LANGUAGE.md     # Language reference
└── cert/
    ├── features/       # BDD feature specifications
    ├── src/            # Test harness library
    └── tests/          # Cucumber test runner
```

## Development

This project uses spec-first BDD development. Feature files in `cert/features/` define the language specification.

Test states:
- **Green**: Implemented correctly
- **Yellow/Pending**: Not implemented yet (backend exits non-zero)
- **Red**: Implemented wrong (backend gives wrong answer)

### Running Tests

```bash
cd cert
cargo test
```

## License

[TODO: Add license]
