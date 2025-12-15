## Summary

The liar-runtime is currently written in Rust and bypasses lIR entirely, violating ADR 019 ("No arrows bypass lIR"). This creates:

1. **Cross-platform barriers**: Platform-specific code (epoll/kqueue) is buried in Rust
2. **ADR violation**: Runtime should compile through lIR like everything else
3. **Portability debt**: Adding new targets (Windows, WASM) requires parallel Rust implementations

## Solution

Two interconnected changes:

### 1. Target-aware compilation

Add a mechanism for liar to compile platform-specific code:

```lisp
(when-target :linux
  (extern epoll_create1 i32 (i32))
  ...)

(when-target :macos
  (extern kqueue i32 ())
  ...)
```

The compiler receives a `--target` flag and only emits code for matching `when-target` blocks.

### 2. Self-hosted runtime in liar

Rewrite liar-runtime as a liar library:

```
lib/
  liar.runtime.liar           ; executor, task queue, waker (portable)
  liar.runtime.linux.liar     ; epoll reactor
  liar.runtime.macos.liar     ; kqueue reactor
  liar.runtime.wasi.liar      ; poll_oneoff reactor (future)
```

The portable parts (executor loop, task management, futures) are pure liar.
Only the thin reactor layer has platform-specific externs.

## Acceptance Criteria

- [ ] `when-target` macro or special form for conditional compilation
- [ ] `--target` flag on liar compiler (linux, macos, wasi, windows)
- [ ] Executor and task queue implemented in liar
- [ ] Waker mechanism implemented in liar  
- [ ] Linux reactor (epoll) implemented in liar
- [ ] macOS reactor (kqueue) implemented in liar
- [ ] liar.io.liar updated to use new runtime
- [ ] liar-runtime Rust crate can be deprecated
- [ ] Feature tests pass on Linux (and macOS if available)

## Benefits

- Fixes ADR 019 violation
- Cross-platform becomes swapping extern declarations
- WASM/WASI support becomes feasible
- Validates liar is expressive enough for systems programming
- Moves toward self-hosting goal (ADR 020)

## Non-goals (for now)

- Windows IOCP support (future work)
- Full WASI async support (future work)
- Removing liar-runtime Rust crate entirely (keep for bootstrapping)
