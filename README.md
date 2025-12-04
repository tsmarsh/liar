# liar

A memory-safe Lisp without garbage collection, compiled to native code via LLVM.

## What is this?

**liar** is a borrow-checked Lisp that compiles through **lIR** (an S-expression assembler for LLVM IR) to produce native executables. It provides Rust-like memory safety guarantees without a garbage collector, while maintaining Lisp's expressive power.

```
liar source → liar compiler → lIR → LLVM IR → native code
```

## Quick Start

```bash
# Build the toolchain
cargo build --release

# Compile a liar program to native executable
./target/release/liarc hello.liar -o hello
./hello

# Or use the REPL for interactive development
./target/release/liar-repl

# For IDE integration, start the nREPL server
./target/release/liar-nrepl
```

## The Two Layers

This project consists of two complementary languages:

### liar - High-Level Lisp (User-Facing)

A full-featured Lisp with:
- **Ownership & borrowing** — compile-time memory safety without GC
- **Type inference** — types inferred automatically
- **Closures** — first-class functions with captured state
- **Atoms** — thread-safe shared state (like Clojure)
- **let vs plet** — explicit single-threaded vs thread-safe bindings
- **Protocols** — polymorphism via protocol dispatch
- **FFI** — call C libraries via `unsafe` blocks

```lisp
; liar example
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun main ()
  (println "10! = {}" (factorial 10)))
```

### lIR - S-Expression LLVM IR (Backend)

A 1:1 mapping to LLVM IR with parentheses. No evaluation model, just LLVM IR syntax:

```lisp
; lIR - direct LLVM IR representation
(define (add i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))
```

## Complete Toolchain

| Tool | Status | What | Input | Output |
|------|--------|------|-------|--------|
| `liarc` | ✅ Done | liar compiler | .liar source | LLVM IR / native |
| `liar-repl` | ✅ Done | Interactive REPL | - | - |
| `liar-nrepl` | ✅ Done | nREPL server | - | IDE integration |
| `lair` | ✅ Done | lIR assembler | .lir source | LLVM IR / native |
| `lir` | ✅ Done | lIR evaluator | lIR expression | Result |
| `lir-repl` | ✅ Done | lIR REPL | - | - |

## Language Features

### Memory Safety Without GC

liar uses ownership and borrowing rules (similar to Rust) enforced at compile time:

```lisp
; Immutable by default
(let ((x 10))
  (let ((y x))  ; x moved to y
    y))         ; x no longer accessible

; Explicit borrowing
(defun use-value (v)
  (println "Value: {}" v))

(let ((x 42))
  (use-value (ref x))   ; shared borrow - x still usable
  (println "x is {}" x))

; Mutable borrowing
(defun increment! (v)
  (set! v (+ v 1)))

(let ((x 10))
  (increment! (ref-mut x))  ; exclusive mutable borrow
  (println "x is now {}" x))
```

### Closures with Analysis

Closures capture their environment and are analyzed for thread-safety:

```lisp
; Closure capturing outer variable
(defun make-adder (n)
  (fn (x) (+ x n)))  ; captures n

(let ((add5 (make-adder 5)))
  (println "{}" (add5 10)))  ; prints 15

; Thread-safe closures with plet
(plet ((counter (atom 0)))
  (defun increment! ()
    (swap! counter (fn (x) (+ x 1)))))
```

### let vs plet - Explicit Concurrency

```lisp
; let - single-threaded, fast, borrows allowed
(let ((x 10)
      (y 20))
  (+ x y))

; plet - thread-safe, all values must be Send+Sync
(plet ((shared-state (atom 0)))
  (spawn (fn () (swap! shared-state inc)))
  (spawn (fn () (swap! shared-state inc))))
```

### Atoms for Shared State

```lisp
(plet ((counter (atom 0)))
  ; swap! applies function atomically
  (swap! counter (fn (x) (+ x 1)))
  
  ; reset! sets new value
  (reset! counter 10)
  
  ; deref gets current value
  (println "Counter: {}" (deref counter)))
```

### Protocols (Polymorphism)

```lisp
; Define a protocol
(defprotocol Stringify
  (to-string [this] String))

; Implement for a type
(defstruct Point (i64 x) (i64 y))

(extend-protocol Stringify Point
  (to-string [p]
    (format "Point({}, {})" (.x p) (.y p))))

; Use it
(let ((p (Point 10 20)))
  (println "{}" (to-string p)))
```

## Examples

### Fibonacci (Recursive)

```lisp
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun main ()
  (println "fib(10) = {}" (fib 10)))
```

### Counter with Atoms

```lisp
(plet ((counter (atom 0)))
  (defun increment! ()
    (swap! counter (fn (x) (+ x 1))))
  
  (defun get-count ()
    (deref counter))
  
  (defun main ()
    (dotimes (i 10)
      (increment!))
    (println "Count: {}" (get-count))))
```

### Map/Filter/Reduce

```lisp
(defun main ()
  (let ((nums [1 2 3 4 5])
        (doubled (map (fn (x) (* x 2)) nums))
        (evens (filter even? nums))
        (sum (reduce + 0 nums)))
    (println "Doubled: {}" doubled)
    (println "Evens: {}" evens)
    (println "Sum: {}" sum)))
```

## Compiler Pipeline

```
liar source
    ↓
Lexer → Tokens
    ↓
Parser → AST
    ↓
Name Resolution → Resolved AST
    ↓
Type Inference → Typed AST
    ↓
Closure Analysis → Annotated AST
    ↓
Ownership Checking → Verified AST
    ↓
Code Generation → lIR AST
    ↓
lIR Codegen → LLVM IR
    ↓
LLVM → Native Code
```

## IDE Integration

The nREPL server provides standard editor integration:

```bash
# Start nREPL server (writes .nrepl-port file)
liar-nrepl

# Or specify port
NREPL_PORT=7888 liar-nrepl
```

Works with:
- VS Code + Calva
- Emacs + CIDER
- Any nREPL-compatible editor

## Compiler Options

### liar Compiler (liarc)

```bash
# Compile to executable
liarc main.liar -o prog

# Emit lIR (intermediate representation)
liarc main.liar --emit-lir -o out.lir

# Optimization levels
liarc main.liar -O3 -o prog

# Link with libraries
liarc main.liar -lm -o prog
```

### lIR Compiler (lair)

```bash
# Compile lIR to executable
lair main.lir -o prog

# Emit LLVM IR
lair main.lir --emit-llvm -o out.ll

# Cross-compilation
lair main.lir --target aarch64-linux-gnu -o prog

# List available targets
lair --print-targets
```

## Architecture Decision Records

The language design is documented in [Architecture Decision Records](doc/adr/):

**Ownership & Memory Safety:**
- [ADR-001](doc/adr/001-immutability-by-default.md) - Immutability by default
- [ADR-002](doc/adr/002-pass-by-reference.md) - Pass by reference semantics
- [ADR-003](doc/adr/003-mutable-reference-sigil.md) - Mutable reference sigil
- [ADR-004](doc/adr/004-lexical-ownership.md) - Lexical ownership
- [ADR-007](doc/adr/007-aliasing-allowed.md) - Aliasing rules

**Closures & Concurrency:**
- [ADR-005](doc/adr/005-closure-captures-ownership.md) - Closure captures
- [ADR-009](doc/adr/009-let-vs-plet.md) - let vs plet
- [ADR-010](doc/adr/010-closure-color.md) - Closure color tracking
- [ADR-011](doc/adr/011-atoms-for-shared-state.md) - Atoms for shared state

**Type System:**
- [ADR-015](doc/adr/015-numeric-primitives.md) - Numeric primitives
- [ADR-016](doc/adr/016-simd-vectors.md) - SIMD vectors
- [ADR-017](doc/adr/017-type-promotion.md) - Type promotion rules

**Collections & Protocols:**
- [ADR-018](doc/adr/018-collections.md) - Collections
- [ADR-022](doc/adr/022-Liar-Core-Protocols) - Core protocols

**Architecture:**
- [ADR-019](doc/adr/019-lir-universal-backend.md) - lIR as universal backend
- [ADR-020](doc/adr/020-toolchain-architecture.md) - Toolchain architecture
- [ADR-021](doc/adr/021-lIR-safety-features.md) - lIR safety features

## Project Structure

```
liar/
├── liar/               # High-level Lisp compiler
│   └── src/
│       ├── lexer.rs    # Tokenization
│       ├── parser.rs   # AST construction
│       ├── resolve.rs  # Name resolution
│       ├── infer.rs    # Type inference
│       ├── closures.rs # Closure analysis
│       ├── ownership.rs# Borrow checking
│       └── codegen.rs  # lIR generation
├── liar-repl/          # Interactive REPL
├── liar-nrepl/         # nREPL server for IDEs
├── liar-cert/          # BDD tests for liar
├── lir-core/           # lIR AST, parser, types
├── lir-codegen/        # LLVM code generation
├── lir-lair/           # lIR AOT compiler
├── lir-cli/            # lIR expression evaluator
├── lir-repl/           # lIR interactive REPL
├── cert/               # BDD tests for lIR
│   └── features/       # Cucumber specifications
├── lib/
│   └── stdlib.liar     # Standard library
└── doc/
    ├── adr/            # Architecture decisions
    ├── LIAR.md         # liar language guide
    └── lIR.md          # lIR reference
```

## Development

Both languages use spec-first BDD development with Cucumber:

```bash
# Run lIR tests (202 scenarios passing)
cargo test --test cert

# Run liar tests
cargo test --test liar-cert
```

### Test States
- **Green**: Implemented correctly
- **Yellow/Pending**: Spec exists, not implemented (exits non-zero)
- **Red**: Implemented incorrectly (wrong answer)

## Implementation Status

### lIR (Backend) - ✅ Complete
- ✅ 202 scenarios passing
- ✅ All LLVM IR features: types, arithmetic, memory, control flow
- ✅ Structs, vectors, atomics
- ✅ FFI support
- ✅ AOT compiler with optimization
- ✅ JIT evaluation
- ✅ REPL

### liar (Frontend) - ✅ Complete
- ✅ Full lexer, parser, AST
- ✅ Name resolution
- ✅ Type inference (Hindley-Milner style)
- ✅ Closure analysis with color tracking
- ✅ Ownership & borrow checking
- ✅ Code generation to lIR
- ✅ AOT compiler
- ✅ REPL with incremental JIT
- ✅ nREPL server for IDE integration

## Documentation

- [liar Language Guide](doc/LIAR.md) - Complete language reference
- [lIR Reference](doc/lIR.md) - LLVM IR assembler documentation
- [Architecture Decision Records](doc/adr/) - Design decisions and rationale
- [Standard Library](lib/stdlib.liar) - Core functions

## Contributing

See the [moths](https://github.com/tsmarsh/moth) in `.moth/` for current work items. This project uses moth for git-friendly issue tracking with speakable 5-character IDs.

```bash
# List current issues
moth ls

# View specific issue
moth show <id>

# Start working on an issue
moth start <id>
```

## License

MIT
