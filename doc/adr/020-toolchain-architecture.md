# ADR 020: Toolchain Architecture

## Status

Accepted

## Context

ADR 019 established that all frontends target lIR. This ADR clarifies what the toolchain looks like and what each component is implemented in.

## Decision

### The Stack

```
liar source
    ↓ liar (Rust)
lIR source  
    ↓ lair (Rust)
LLVM IR
    ↓ LLVM
native
```

### Naming

| Tool | What | Mnemonic |
|------|------|----------|
| `liar` | high-level compiler | "liar" the language |
| `lair` | lIR assembler | anagram of liar/lIR; where the low-level stuff lives |
| `lIR` | intermediate representation | "liar IR" |

### Components

| Component | Written in | Input | Output |
|-----------|------------|-------|--------|
| lair | Rust | lIR source | LLVM IR |
| liar | Rust | liar source | lIR source |
| LLVM | C++ | LLVM IR | native |

### lair — The lIR Assembler

The foundation. Responsibilities:

- Parse S-expressions
- Validate types match (no implicit promotion)
- Validate operations are valid for types
- Emit LLVM IR (via inkwell or llvm-sys)
- Report errors with source locations

This is the bedrock — analogous to an assembler for a physical CPU.

```bash
echo '(add (i32 5) (i32 6))' | lair --run
# => 11
```

### liar — The Compiler

Built on top of lair. Responsibilities:

- Parse liar source
- Macro expansion
- Type inference
- Borrow checking
- Closure color tracking (let vs plet)
- Ownership analysis
- Emit lIR

```bash
echo '(+ 5 6)' | liar | lair --run
# => 11
```

### Why Rust?

Both components are written in Rust because:

1. **Memory safety**: Compilers are complex; Rust prevents bugs
2. **LLVM bindings**: inkwell/llvm-sys are mature
3. **Tooling**: cargo, clippy, testing infrastructure
4. **Performance**: Compilers should be fast

### Future: Self-Hosting

The standard bootstrap path if we want to self-host:

1. ✓ Write lair (lIR assembler) in Rust
2. Write liar compiler v1 in Rust → emits lIR
3. Write liar compiler v2 in liar → emits lIR
4. Compile v2 with v1
5. Retire Rust-based v1 (or keep for bootstrapping)

Self-hosting is optional but validates that liar is expressive enough to write real programs.

### Build Flow

**Development:**

```bash
# Build the toolchain
cargo build --release

# Compile liar to native
liar input.liar | lair -o output
./output

# Or compile to lIR for inspection
liar input.liar > output.lir
cat output.lir
lair output.lir -o output
```

**Direct execution:**

```bash
liar input.liar | lair --run
```

**REPL:**

```bash
liar
> (+ 5 6)
11
> (defun square (x) (* x x))
> (square 7)
49
```

Under the hood, REPL still goes through lIR:

```
input → liar → lIR → lair → LLVM JIT → execute
```

## Consequences

### Positive

- **Clear separation**: Assembler and compiler are distinct
- **Testable layers**: Test lair independently of liar
- **Familiar tooling**: Rust ecosystem for development
- **Single backend**: Per ADR 019, everything goes through lIR
- **Good names**: liar/lair are memorable and related

### Negative

- **Two Rust projects**: More code to maintain
- **Not self-hosted initially**: liar can't compile itself at first

### Neutral

- Standard approach for new languages
- Self-hosting can come later if desired
