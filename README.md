# liar

A memory-safe Lisp without garbage collection, compiled to native code via LLVM.

## What is this?

**liar** is a borrow-checked Lisp that compiles through **lIR** (an S-expression assembler for LLVM IR) to produce native executables. It aims to provide Rust-like memory safety guarantees without a garbage collector, while maintaining Lisp's expressive power.

```
liar source → liar compiler → lIR → LLVM IR → native code
```

## Current Status

liar is a working compiler with a functional REPL. The core language is complete with a growing standard library.

**What works today (117 passing test scenarios for liar, 286 for lIR):**
- Arithmetic, comparisons, boolean logic (int and float)
- Functions with type inference and annotations
- Let bindings (sequential and parallel, with destructuring)
- Atoms for thread-safe shared state
- Closures with capture analysis and escape detection
- Structs with typed fields and protocols with default implementations
- Macros with quasiquote, unquote, splicing, and gensym
- Ownership and simplified borrow checking
- I/O via `print`/`println` builtins
- Async I/O (file, socket, pipe) via `liar.io`
- Persistent data structures (vectors, hashmaps, hashsets)
- REPL with incremental JIT
- nREPL server for IDE integration

**Not yet implemented:**
- Pattern matching / match expressions
- Algebraic data types (enums)
- Module system
- Full borrow checking

## Quick Start

```bash
# Build the toolchain
cargo build --release

# Use the REPL
./target/release/liar-repl

liar> (+ 1 2)
3
liar> (defun square (x) (* x x))
defined: square
liar> (square 5)
25
```

## The Two Layers

### liar - High-Level Lisp

A Lisp with ownership tracking and type inference:

```lisp
;; Functions
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Let bindings
(let ((x 10)
      (y 20))
  (+ x y))

;; Closures
(let ((add5 (fn (x) (+ x 5))))
  (add5 10))  ; => 15
```

### lIR - S-Expression LLVM IR

A 1:1 mapping to LLVM IR with parentheses:

```lisp
(define (add i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))
```

## Language Features

### Basic Expressions

```lisp
;; Arithmetic
(+ 1 2)           ; => 3
(- 10 3)          ; => 7
(* 4 5)           ; => 20
(/ 20 4)          ; => 5
(rem 17 5)        ; => 2

;; Comparisons
(< 1 2)           ; => true
(>= 5 5)          ; => true
(= 3 3)           ; => true
(!= 1 2)          ; => true

;; Boolean logic
(and true false)  ; => false
(or true false)   ; => true
(not false)       ; => true
```

### Functions

```lisp
;; Named functions
(defun add (a b)
  (+ a b))

(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; With type annotations
(defun add-typed (a: i64 b: i64) -> i64
  (+ a b))

;; Closures
(let ((multiplier 3))
  (fn (x) (* x multiplier)))
```

### Let Bindings

```lisp
;; Sequential binding
(let ((x 10)
      (y (+ x 5)))   ; y can reference x
  (* x y))           ; => 150

;; Parallel binding (thread-safe, bindings independent)
(plet ((a 1)
       (b 2))
  (+ a b))
```

### Atoms (Thread-Safe Mutable State)

```lisp
;; Create an atom
(let ((counter (atom 0)))
  ;; Read with @
  @counter              ; => 0
  
  ;; Update atomically with swap!
  (swap! counter inc)   ; applies inc, returns new value
  @counter              ; => 1
  
  ;; Set directly with reset!
  (reset! counter 100)
  @counter)             ; => 100

;; Compare-and-set for lock-free algorithms
(let ((x (atom 0)))
  (compare-and-set! x 0 1))  ; => true if x was 0
```

### Structs

```lisp
;; Define a struct
(defstruct Point
  x: i64
  y: i64)

;; Field access uses (. expr field)
(let ((p (Point 10 20)))
  (. p x))              ; => 10
```

### Protocols

```lisp
;; Define a protocol
(defprotocol Describable
  (describe [self]))

;; Extend for a type
(extend-protocol Describable Point
  (describe [self]
    42))  ; placeholder - no string ops yet
```

### Collections (Literals)

```lisp
;; Persistent vector
[1 2 3]

;; Persistent map  
{:a 1 :b 2}

;; Keywords
:foo

;; SIMD vectors
<<1 2 3 4>>           ; 4x i64
<<1.0 2.0 3.0 4.0>>   ; 4x f64
```

### Control Flow

```lisp
;; If (always requires else branch)
(if (> x 0)
    "positive"
    "non-positive")

;; Do block (sequence, returns last)
(do
  (swap! counter inc)
  (swap! counter inc)
  @counter)
```

### Ownership & Borrowing

```lisp
;; Borrow (read-only reference)
(ref x)

;; Mutable borrow  
(ref-mut x)

;; The compiler tracks ownership and prevents:
;; - Use after move
;; - Multiple mutable borrows
;; - Mutable borrow while immutably borrowed
```

## Toolchain

| Tool | Description |
|------|-------------|
| `liar-repl` | Interactive REPL with incremental JIT |
| `liar-nrepl` | nREPL server for IDE integration |
| `lair` | lIR to native compiler |
| `lir` | lIR expression evaluator |
| `lir-repl` | lIR interactive REPL |

## Standard Library

Libraries in `lib/` use namespaced naming (`liar.{name}.liar`):

```lisp
;; Core utilities (liar.prelude)
(inc x)           ; (+ x 1)
(dec x)           ; (- x 1)
(min a b)         ; smaller of a, b
(max a b)         ; larger of a, b
(abs x)           ; absolute value
(clamp x lo hi)   ; constrain x to [lo, hi]

;; Threading macros
(thread-first x (f a) (g b))  ; => (g (f x a) b)
(thread-last x (f a) (g b))   ; => (g b (f a x))

;; I/O (liar.io)
(slurp "file.txt")            ; read file to string
(spit "file.txt" contents)    ; write string to file
(println-str "hello")         ; print with newline

;; Collections (liar.vector, liar.hashmap, liar.hashset)
(vector 1 2 3)                ; persistent vector
(hashmap :a 1 :b 2)           ; persistent hash map
(hashset 1 2 3)               ; persistent hash set
```

**Built-in operations** (no require needed):
- Arithmetic: `+`, `-`, `*`, `/`, `rem`
- Comparison: `<`, `>`, `<=`, `>=`, `=`, `!=`
- Boolean: `and`, `or`, `not`
- Atoms: `atom`, `swap!`, `reset!`, `compare-and-set!`, `@`
- I/O: `print`, `println`
- Memory: `heap-array`, `aget`, `aset`, `share`
- Pointers: `ptr+`, `load-byte`, `store-byte`

## Project Structure

```
liar/
├── liar/           # High-level Lisp compiler
│   └── src/
│       ├── lexer.rs, parser.rs, ast.rs    # Frontend
│       ├── expand.rs, resolve.rs          # Macro expansion, name resolution
│       ├── infer.rs, ownership.rs         # Type inference, borrow checking
│       ├── closures/                      # Closure analysis & conversion
│       └── codegen/                       # lIR code generation
├── liar-repl/      # Interactive REPL with JIT
├── liar-nrepl/     # nREPL server for IDE integration
├── liar-runtime/   # Async I/O runtime (epoll/kqueue)
├── liar-cert/      # BDD tests for liar (117 scenarios)
├── lir-core/       # lIR AST, parser, types
├── lir-codegen/    # LLVM IR generation, JIT
├── lir-lair/       # lIR AOT compiler
├── lir-cli/        # lIR expression evaluator
├── lir-repl/       # lIR REPL
├── cert/           # BDD tests for lIR (286 scenarios)
├── lib/            # Standard library (.liar files)
└── doc/            # Documentation
    ├── LIAR.md     # Language specification
    ├── lIR.md      # lIR reference
    └── adr/        # Architecture decisions
```

## Development

```bash
# Run all tests
cargo test

# Run lIR specification tests (286 scenarios)
cargo test --test cert

# Run liar tests (117 scenarios)
cargo test -p liar-cert --test cert
```

## Documentation

- [doc/LIAR.md](doc/LIAR.md) - Language specification (note: describes planned features, not all implemented)
- [doc/lIR.md](doc/lIR.md) - lIR reference (fully implemented)
- [doc/adr/](doc/adr/) - Architecture Decision Records

## Roadmap

Near-term priorities:
1. **Pattern matching** - `match` expressions with destructuring
2. **Algebraic data types** - enums / sum types
3. **Module system** - proper imports/exports
4. **String protocols** - Unicode strings with Seq protocol

See `.moth/` for detailed work items.

## License

MIT
