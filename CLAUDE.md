# lIR + liar

Two languages, one goal: a memory-safe Lisp without garbage collection.

```
liar source → liar → lIR → lair → LLVM IR → native
```

## The Toolchain

| Tool | Status | What | Input | Output |
|------|--------|------|-------|--------|
| `lair` | **Done** | lIR assembler | lIR source | LLVM IR / native |
| `lir` | **Done** | Expression evaluator | lIR expression | Result |
| `lir-repl` | **Done** | Interactive REPL | - | - |
| `liar` | **Done** | High-level compiler | liar source | lIR |
| `liarliar` | **In Progress** | Self-hosted compiler | liar source | lIR |
| `liar-repl` | **Done** | Interactive REPL with JIT | - | - |
| `liar-nrepl` | **Done** | nREPL server for IDE integration | - | - |

## The Two Layers

| Layer | What it is | Status | File |
|-------|------------|--------|------|
| **lIR** | S-expression assembler for LLVM IR. 1:1 mapping, no sugar. | **Implemented** | [lIR.md](doc/lIR.md) |
| **liar** | Borrow-checked Lisp with closures, atoms, threading. User-facing. | **Implemented** | [LIAR.md](doc/LIAR.md) |

## Current Capabilities

### lIR

lIR is fully functional as an LLVM IR assembler:
- All scalar and vector types (i1-i64, float, double, ptr, vectors)
- All arithmetic, bitwise, and comparison operations
- Type conversions (trunc, zext, sext, fptrunc, fpext, etc.)
- Memory operations (alloca, load, store, getelementptr)
- Control flow (branches, phi nodes, function calls)
- Structs (defstruct, field access via GEP)
- FFI (external function declarations)
- AOT compilation to native executables

### liar

liar is a working compiler (117 passing liar-cert scenarios):
- Arithmetic, comparisons, boolean logic (int and float)
- Functions with type inference and annotations
- Let bindings (sequential and parallel, with destructuring)
- Atoms for thread-safe shared state
- Closures with capture analysis and escape detection
- Structs with typed fields and field access via `.`
- Protocols with runtime dispatch and default implementations
- Macros with quasiquote, unquote, splicing, and gensym
- Tail call optimization
- FFI to C functions (extern declarations)
- I/O via `print`/`println` builtins and `lib/liar.io.liar`
- Type conversions (trunc, zext, sext, fptosi, sitofp, etc.)
- Bitwise operations and popcount
- Byte-level pointer operations (store-byte, load-byte, ptr+)
- REPL with incremental JIT
- Async I/O with reactor-based event loop

**Standard Library (`lib/`):**
- `liar.prelude.liar` — Core functions (inc, dec, min, max, etc.) and threading macros
- `liar.seq.liar` — Protocols (Seq, Countable, Indexable) and cons-based lists
- `liar.vector.liar` — Persistent vectors (32-way trie)
- `liar.hashmap.liar` — Persistent hash maps (HAMT)
- `liar.hashset.liar` — Persistent hash sets
- `liar.io.liar` — Async file I/O (slurp, spit, read-line), sockets, pipes
- `liar.array.liar` — Low-level array helpers (array-clone, array-append)
- `liar.runtime.liar` — FFI declarations for liar-runtime (async executor)
- `liar.async.liar` — Async task primitives (spawn, block-on)
- `liar.mut-vector.liar` — Mutable vector for building persistent vectors
- `liar.test.liar` — Unit test assertions (assert-eq, assert-lt, run-tests)

**Not yet implemented:**
- Pattern matching / match expressions
- Algebraic data types (enums)
- Module system
- Full borrow checking (simplified ownership model)
- Garbage collection

## Build and Test

Prefer `make` targets for repeatable builds:
- `make` builds `liarc`, `lair`, and `liarliar` into `target/release/`.
- `make liar-spec` runs the liar -> lIR spec suite and writes `target/liar-spec.ok`.
- `make test` runs `make liar-spec` plus `cargo test`.
- `make lint` runs `liar-lint` over `lib/*.liar` and `liarliar/*.liar`.

---

# lIR

An S-expression assembler for LLVM IR. Not a Lisp—just LLVM IR in parens.

## Philosophy

**Spec-first BDD development.** Feature files define the language specification. Tests have three states:
- **Green**: Implemented correctly
- **Yellow/Pending**: Not implemented yet (runtime exits non-zero)
- **Red**: Implemented wrong (runtime gives wrong answer)

The distinction between "not yet implemented" and "broken" is critical. A non-zero exit code from the runtime means WIP, not failure.

## Core Design

This is a 1:1 mapping to LLVM IR. No sugar, no promotion, no Lisp semantics. S-expressions are just the syntax.

```lisp
(fadd (double 5.0) (double 6.0))
```

Maps directly to:
```llvm
%0 = fadd double 5.0, 6.0
```

### Types

Use LLVM's type names exactly:

- **Integers:** `i1`, `i8`, `i16`, `i32`, `i64`
- **Floats:** `float`, `double`
- **Vectors:** `<4 x i32>`, `<2 x double>`, etc.

No `bool`—use `i1`. No `f32`/`f64`—use `float`/`double`.

```lisp
(i1 1)        ; true
(i1 0)        ; false
(i32 42)
(double 3.14)
```

### No Type Promotion

Types must match exactly. This is an error:
```lisp
(add (i8 1) (i32 2))  ; ERROR: type mismatch
```

You must explicitly convert:
```lisp
(add (sext i32 (i8 1)) (i32 2))  ; OK
```

### Operations

**Integer Arithmetic:**
- `add`, `sub`, `mul`
- `sdiv`, `udiv` (signed/unsigned division)
- `srem`, `urem` (signed/unsigned remainder)

**Float Arithmetic:**
- `fadd`, `fsub`, `fmul`, `fdiv`, `frem`

**Bitwise:**
- `and`, `or`, `xor`
- `shl` (shift left)
- `lshr` (logical shift right)
- `ashr` (arithmetic shift right)

**Integer Comparison (icmp):**
```lisp
(icmp eq (i32 5) (i32 5))   ; => (i1 1)
(icmp ne (i32 5) (i32 6))   ; => (i1 1)
(icmp slt (i32 -1) (i32 1)) ; => (i1 1) signed less than
(icmp ult (i32 -1) (i32 1)) ; => (i1 0) unsigned less than (-1 is MAX_UINT)
```
Predicates: `eq`, `ne`, `slt`, `sle`, `sgt`, `sge`, `ult`, `ule`, `ugt`, `uge`

**Float Comparison (fcmp):**
```lisp
(fcmp oeq (double 1.0) (double 1.0))  ; => (i1 1) ordered equal
(fcmp olt (double 1.0) (double 2.0))  ; => (i1 1) ordered less than
(fcmp uno (double nan) (double 1.0))  ; => (i1 1) unordered (either is NaN)
```
Predicates: `oeq`, `one`, `olt`, `ole`, `ogt`, `oge`, `ord`, `uno`, `ueq`, `une`, `ult`, `ule`, `ugt`, `uge`

**Conversions:**
- `trunc` — truncate to smaller int
- `zext` — zero extend to larger int
- `sext` — sign extend to larger int
- `fptrunc` — truncate to smaller float
- `fpext` — extend to larger float
- `fptoui` — float to unsigned int
- `fptosi` — float to signed int
- `uitofp` — unsigned int to float
- `sitofp` — signed int to float

**Select:**
```lisp
(select (icmp slt (i32 5) (i32 10)) (i32 1) (i32 2))  ; => (i32 1)
```

**Vectors:**
- `extractelement`
- `insertelement`
- `shufflevector`

## What lIR Is Not

- No `if`/`then`/`else` — use `select`, or eventually `br`/`phi`/blocks
- No implicit conversions
- No `bool` type
- No operator overloading (int `add` vs float `fadd`)
- No `and`/`or` on booleans — use bitwise `and`/`or` on `i1`

---

# liar

See [LIAR.md](doc/LIAR.md) for the full language definition.

**Key concepts:**

| Concept | Summary |
|---------|---------|
| Functions | `defun` with optional type annotations (`-> i64`) |
| Closures | `fn` with automatic capture analysis |
| Structs | `defstruct` with typed fields, field access via `.` |
| Protocols | `defprotocol` / `extend-protocol` for polymorphism |
| Macros | `defmacro` with quasiquote, unquote, splicing |
| Threading | `let` vs `plet` (parallel bindings with atoms) |
| Memory | `share` for heap allocation, manual management |
| FFI | `extern` declarations for C functions |

**Design decisions:** See [doc/adr/](doc/adr/) for Architecture Decision Records.

---

# Compiler Architecture

## liar Compiler Pipeline

The liar compiler (`liar/src/`) processes source through these phases:

1. **Lexer** (`lexer.rs`) — Tokenizes source into tokens
2. **Parser** (`parser.rs`) — Builds AST from tokens
3. **Macro Expansion** (`expand.rs`) — Expands macros, handles quasiquote/unquote
4. **Name Resolution** (`resolve.rs`) — Resolves symbols, registers builtins
5. **Type Inference** (`infer.rs`) — Hindley-Milner style inference with annotations
6. **Closure Analysis** (`closures/`) — Capture analysis, escape detection, conversion
7. **Ownership Checking** (`ownership.rs`) — Simplified borrow checking
8. **Code Generation** (`codegen/`) — Emits lIR AST

## Key Compiler Modules

| Module | Responsibility |
|--------|----------------|
| `ast.rs` | AST types (Expr, Def, Pattern) |
| `closures/analysis.rs` | Determines what variables closures capture |
| `closures/escape.rs` | Detects if closures escape their scope |
| `closures/conversion.rs` | Converts closures to structs + function pointers |
| `codegen/builtins.rs` | Generates lIR for builtin operations (+, -, print, etc.) |
| `codegen/protocols.rs` | Protocol dispatch table generation |
| `codegen/context.rs` | Code generation context and state |
| `eval.rs` | Macro-time evaluation (for defmacro bodies) |
| `loader.rs` | File loading with namespace support |
| `macro_jit.rs` | JIT compilation for macro expansion |

## lIR Crates

| Crate | Purpose |
|-------|---------|
| `lir-core` | AST, parser, types, borrow checking for lIR |
| `lir-codegen` | LLVM IR generation, JIT compilation |
| `lir-lair` | AOT compiler (lIR → native executable) |
| `lir-cli` | Command-line lIR expression evaluator |
| `lir-repl` | Interactive lIR REPL |

## Runtime

`liar-runtime/` provides:
- Async I/O reactor (epoll on Linux, kqueue on macOS)
- Task executor for async/await
- FFI functions callable from liar code

---

# Coding Standards

These standards keep files manageable and code maintainable.

## File Size Limits

| Metric | Limit | Action if exceeded |
|--------|-------|-------------------|
| Lines per file | 500 | Split into modules |
| Functions per file | 15 | Split by responsibility |
| Lines per function | 50 | Extract helpers |
| Nesting depth | 3 | Flatten or extract |

## Key Rules

### One Responsibility Per File
If you need section comments like `// ========== SECTION ==========` to navigate, split the file.

### No Thread-Local State for Pass Data
Thread-locals make testing hard and hide dependencies.

**Bad:**
```rust
thread_local! {
    static CAPTURED_VARS: RefCell<HashMap<...>> = ...
}
```

**Good:**
```rust
struct AnalysisContext {
    captured_vars: HashMap<...>,
}
```

### Passes Take and Return Data
Each compiler pass should be a pure transformation.

**Bad:**
```rust
fn analyze(program: &Program) {
    // mutates global state
}
```

**Good:**
```rust
fn analyze(program: &Program) -> AnalysisResult {
    // returns result
}
```

### Explicit Dependencies
All functions take context explicitly, no hidden globals.

```rust
fn generate_expr(ctx: &mut Context, expr: &Expr) -> Result<...>
```

### lIR Boundary
lIR is a stable compilation target. Keep it generic:
- No liar imports in `lir-core` or `lir-codegen`
- No liar terminology ("closure", "protocol", "atom") in lIR
- All abstraction lives in liar, codegen just translates

## Testing

- Unit tests live with the code they test (`#[cfg(test)]` modules)
- Integration tests use feature files (BDD style)
- Tests should be fast and isolated

---

# Moth Agent Guide

This guide helps LLM agents work effectively with moth, a git-based file issue tracker.

## Overview

Moth stores issues as markdown files in `.moth/` directories organized by status (ready, doing, done). Each issue has a unique ID, severity, and slug derived from the title.

## File Structure

```
.moth/
├── config.yml          # Project configuration
├── .current            # Current issue ID (when working on an issue)
├── ready/              # Issues ready to start
│   └── {id}-{severity}-{slug}.md
├── doing/              # Issues in progress
│   └── {id}-{severity}-{slug}.md
└── done/               # Completed issues
    └── {id}-{severity}-{slug}.md
```

Prioritized issues have a numeric prefix: `001-{id}-{severity}-{slug}.md`

## Workflow Commands

### Viewing Issues

```bash
# List all active issues (excludes done)
moth ls

# List issues in specific status
moth ls -t ready
moth ls -t doing

# List all issues including done
moth ls -a

# Filter by severity
moth ls -s high
moth ls -s crit

# Show current issue details
moth show

# Show specific issue
moth show {id}
```

### Working on Issues

```bash
# Start working on an issue (moves to doing, sets as current)
moth start {id}

# Mark issue as done
moth done {id}

# Mark current issue as done
moth done

# Move issue to any status
moth mv {id} {status}
```

### Creating Issues

```bash
# Create new issue (opens editor)
moth new "Fix login bug"

# Create with severity
moth new "Critical security fix" -s crit

# Create without opening editor
moth new "Quick fix" --no-edit

# Create and immediately start working
moth new "Urgent task" --start
```

### Issue Management

```bash
# Edit issue content
moth edit {id}

# Delete issue
moth rm {id}

# Change severity
moth severity {id} high
```

### Priority Management

```bash
# Set priority number
moth priority {id} 1

# Move to top priority
moth priority {id} top

# Move to bottom (removes priority)
moth priority {id} bottom

# Position relative to another issue
moth priority {id} above {other_id}
moth priority {id} below {other_id}

# Renumber priorities sequentially
moth compact
moth compact ready
```

## Severity Levels

From highest to lowest:
- `crit` - Critical, must fix immediately
- `high` - High priority
- `med` - Medium priority (default)
- `low` - Low priority

## Partial ID Matching

All commands accept partial IDs. If you have issue `abc12`, you can use:
- `moth show abc12` (full)
- `moth show abc1` (partial)
- `moth show a` (if unambiguous)

## Git Integration

### Commit Hook

Moth can auto-prefix commit messages with the current issue ID:

```bash
# Install the hook
moth hook install

# With existing hook
moth hook install --append

# Remove hook
moth hook uninstall
```

When active, commits are prefixed: `[abc12] Your commit message`

### Commit Message Format

When committing changes related to an issue, prefix with the issue ID:

```bash
git commit -m "[abc12] Fix authentication bypass"
```

This links commits to issues in the report.

## Generating Reports

```bash
# Full history as CSV
moth report

# From specific commit
moth report --since abc123

# Between commits
moth report --since abc123 --until def456
```

Output includes: commit info, story changes (created, moved, edited, deleted), and code commits referencing issues.

## Agent Best Practices

### Starting Work

1. Check current issues: `moth ls`
2. Find issue to work on or check current: `moth show`
3. Let team mates know what you are working on by `moth start {id}`, and then commiting and pushing
3. Start working

### During Development

1. Make changes and commit frequently
2. Keep issue content updated if requirements change
3. Commit code frequently. 

### Completing Work

1. Ensure all changes committed
2. Mark issue done: `moth done`
3. The `.current` file is automatically cleared

### Creating New Issues

When user requests new work:
1. Create issue: `moth new "Title" -s {severity} --no-edit`
2. Optionally start immediately with `--start` flag
3. Update issue file with detailed requirements if needed

IMPORTANT: Do not start work unless you know which moth it is associated with, and prompt to start a new moth if you believe the request is out of scope of the current moth.

### Checking Status

```bash
# Quick status check
moth ls

# What am I working on?
moth show

# Full project state
moth ls -a
```

## Configuration Reference

`.moth/config.yml`:

```yaml
statuses:
  - name: ready
    dir: ready
    prioritized: true    # Enable priority ordering
  - name: doing
    dir: doing
  - name: done
    dir: done

default_severity: med    # Default for new issues
editor: vi               # Editor for moth edit
id_length: 5             # Length of generated IDs
no_edit: false           # Skip editor on moth new

priority:
  auto_compact: false    # Auto-renumber after priority changes
```

## Common Patterns

### Pick up next priority issue
```bash
moth ls -t ready
moth start {first-id}
```

### Quick bug fix
```bash
moth new "Fix typo in header" -s low --no-edit --start
# make fix
git commit -m "[{id}] Fix typo"
moth done
```

### Triage incoming work
```bash
moth new "Investigate performance issue" -s med --no-edit
moth priority {id} top
```

### Review what was done
```bash
moth ls -t done
moth report --since HEAD~10
```
