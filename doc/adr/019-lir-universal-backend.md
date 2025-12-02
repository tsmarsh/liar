# ADR 019: lIR as Universal Backend

## Status

Accepted

## Context

When building a compiler with multiple frontends or execution modes (AOT, JIT, REPL), there's a risk of implementation drift. In Consair, keeping AOT and JIT synchronized was difficult because they had separate code paths.

We have two layers:
- **lIR**: S-expression assembler for LLVM IR
- **liar**: Borrow-checked Lisp

The question: should liar compile to lIR, or directly to LLVM IR?

## Decision

**All frontends must target lIR. lIR is the single backend.**

"Target lIR" means the **output** is lIR, not that the compiler is **written in** lIR. See ADR 020 for toolchain implementation details.

```
liar source → liar compiler → lIR → LLVM IR → native/JIT
```

If lIR is missing a feature needed to implement something, that is a **bug in lIR**, not a reason to bypass lIR and drop to Rust/LLVM directly.

### The Rule

```
┌─────────────┐
│   liar      │
│  (or other  │──────┐
│  frontends) │      │
└─────────────┘      │
                     ▼
              ┌─────────────┐
              │    lIR      │  ← single source of truth
              └─────────────┘
                     │
                     ▼
              ┌─────────────┐
              │  LLVM IR    │
              └─────────────┘
                     │
                     ▼
              ┌─────────────┐
              │   native    │
              └─────────────┘
```

No arrows bypass lIR.

### Execution Modes

All modes share the same backend:

| Mode | Path |
|------|------|
| AOT compile | `liar source → lIR → LLVM → binary` |
| JIT execute | `liar source → lIR → LLVM → run` |
| REPL eval | `liar expr → lIR → LLVM → execute` |
| Macro expansion | `liar macro → lIR → LLVM → run at compile time` |

Because they all go through lIR → LLVM, there's one codegen to test and trust.

### What This Means in Practice

**Correct approach:**

```
"liar needs tail call optimization"
  → implement TCO in lIR
  → liar emits lIR with TCO
  → all frontends get TCO
```

**Wrong approach:**

```
"liar needs tail call optimization"
  → bypass lIR, emit LLVM directly from Rust
  → now there are two backends
  → drift begins
```

### lIR Completeness

lIR must expose everything LLVM IR offers that's needed for language implementation:

- All LLVM instructions
- Function definitions and calls
- Control flow (branches, phi nodes)
- Memory operations (alloca, load, store, GEP)
- Intrinsics (memcpy, llvm.expect, etc.)
- Metadata (debug info, optimization hints)
- Module structure

If LLVM has it and a frontend needs it, lIR must expose it.

## Consequences

### Positive

- **One backend**: Single source of truth, no drift
- **Testable**: Exhaustively test lIR, trust composition
- **Debuggable**: Inspect lIR output at any point
- **Consistent**: AOT, JIT, REPL behave identically
- **Portable**: New frontends automatically work

### Negative

- **lIR must be complete**: Missing features block frontends
- **Extra layer**: One more translation step
- **lIR maintenance**: Must keep up with LLVM changes

### Neutral

- lIR stays 1:1 with LLVM IR (no sugar), so completeness is tractable
- The translation step is mechanical, not complex

## Testing Strategy

1. **lIR tests**: Exhaustive feature file coverage of lIR→LLVM
2. **liar→lIR tests**: Verify correct lIR emission
3. **Trust composition**: If both pass, the whole pipeline works

No separate AOT vs JIT test suites needed. They're the same code path.
