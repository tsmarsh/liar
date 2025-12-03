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

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile lIR source to native executable
./target/release/lair hello.lir -o hello
./hello

# Or use the REPL for interactive exploration
./target/release/lir-repl
```

### Example: Fibonacci

```lisp
; fib.lir - Recursive Fibonacci

(declare atoi i32 (ptr))
(declare printf i32 (ptr i64))

(define (fib i64) ((i64 n))
  (block entry
    (br (icmp sle n (i64 1)) base recurse))
  (block base
    (ret n))
  (block recurse
    (let ((n1 (call @fib (sub n (i64 1))))
          (n2 (call @fib (sub n (i64 2)))))
      (ret (add n1 n2)))))

(define (main i32) ((i32 argc) (ptr argv))
  (block entry
    (let ((argv1-ptr (getelementptr ptr argv (i64 1)))
          (argv1 (load ptr argv1-ptr))
          (n (sext i64 (call @atoi argv1)))
          (result (call @fib n)))
      (call @printf (string "%lld\n") result)
      (ret (i32 0)))))
```

```bash
$ lair fib.lir -o fib
$ ./fib 10
55
```

## Tools

| Tool | Description |
|------|-------------|
| `lair` | AOT compiler - compiles lIR to native executables |
| `lir` | Expression evaluator - evaluates single expressions |
| `lir-repl` | Interactive REPL for exploration |

### lair - The Compiler

```bash
# Compile and link
lair main.lir -o prog

# Compile only (produce .o)
lair main.lir -c -o main.o

# Emit LLVM IR
lair main.lir --emit-llvm -o out.ll

# Emit assembly
lair main.lir -S -o out.s

# Optimization levels
lair main.lir -O3 -o prog

# Link with libraries
lair main.lir -lm -o prog

# Cross-compilation
lair main.lir --target aarch64-linux-gnu -o prog

# List available targets
lair --print-targets
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

; Pointers
ptr                 ; opaque pointer type

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

### Memory

```lisp
; Stack allocation
(alloca i64)                ; allocate space for i64
(alloca i32 (i32 10))       ; allocate array of 10 i32s

; Load and store
(load i64 ptr)              ; load i64 from pointer
(store (i64 42) ptr)        ; store 42 to pointer

; Pointer arithmetic
(getelementptr i8 ptr (i64 5))           ; ptr + 5 bytes
(getelementptr %struct.point ptr (i32 0) (i32 1))  ; field access
```

### Control Flow

```lisp
; Unconditional branch
(br label)

; Conditional branch
(br (icmp slt x (i64 0)) negative positive)

; Phi nodes (SSA merge)
(phi i64 (block1 val1) (block2 val2))

; Select (ternary)
(select (icmp slt x y) x y)  ; min(x, y)
```

### Functions

```lisp
; Define a function
(define (add2 i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))

; Call a function
(call @add2 (i64 3) (i64 4))  ; => (i64 7)

; External declaration (FFI)
(declare printf i32 (ptr i64))
(call @printf (string "Value: %lld\n") (i64 42))
```

### Structs

```lisp
; Define a struct type
(defstruct point (i64 i64))

; Access struct fields via GEP
(getelementptr %struct.point ptr (i32 0) (i32 0))  ; field 0
(getelementptr %struct.point ptr (i32 0) (i32 1))  ; field 1
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

- [Language Guide](doc/lIR.md) — comprehensive reference
- [Architecture Decision Records](doc/adr/) — design decisions

## Project Structure

```
lir/
├── lir-core/       # AST, parser, types
├── lir-codegen/    # LLVM code generation
├── lir-lair/       # AOT compiler binary (lair)
├── lir-cli/        # Expression evaluator (lir)
├── lir-repl/       # Interactive REPL
├── cert/           # BDD test specifications
│   └── features/   # Cucumber feature files
└── doc/            # Documentation
```

## Development

This project uses spec-first BDD development. Feature files in `cert/features/` define the language specification.

Test states:
- **Green**: Implemented correctly
- **Yellow/Pending**: Not implemented yet (runtime exits non-zero)
- **Red**: Implemented wrong (runtime gives wrong answer)

### Running Tests

```bash
cargo test --test cert
```

### Current Status

202 scenarios passing, covering:
- All scalar and vector types
- Arithmetic, bitwise, and comparison operations
- Type conversions
- Memory operations (alloca, load, store, GEP)
- Control flow (branches, phi nodes)
- Functions (definition, calls, recursion)
- Structs and field access
- FFI (external declarations)

## License

MIT
