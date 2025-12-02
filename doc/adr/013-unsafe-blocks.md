# ADR 013: unsafe Blocks for FFI

## Status

Accepted

## Context

Corsair needs to interact with the outside world:
- System calls (file I/O, networking)
- C libraries
- Raw memory operations

These operations cannot be verified by the compiler. We need an escape hatch.

Options:
1. **No FFI**: Too restrictive, can't build real programs
2. **Implicit unsafe**: All FFI is allowed, safety holes everywhere
3. **Explicit unsafe blocks**: FFI requires opt-in, contained danger zones

## Decision

Use **`unsafe` blocks** for operations the compiler cannot verify.

```lisp
(unsafe
  (call @strlen (to-cstring s)))
```

**What `unsafe` enables:**
- FFI calls to C functions
- Raw pointer operations
- Syscalls
- Accessing raw memory

**What `unsafe` does NOT disable:**
- Ownership tracking
- Closure color tracking
- Type checking
- Borrow rules for `let` closures

```lisp
(unsafe
  (let ((x [1 2 3]))
    (let ((y x))              ; x moved to y — still enforced!
      (call @c_func x))))     ; ERROR: x was moved
```

## Consequences

### Positive

- **Contained danger**: Unsafe code is visually marked
- **Audit trail**: `grep unsafe` finds all dangerous code
- **Safe wrappers**: Unsafe internals can present safe APIs
- **Compiler still helps**: Type system and ownership still work

### Negative

- **Programmer responsibility**: Compiler can't verify correctness in unsafe
- **Potential for bugs**: Use-after-free, buffer overflow, etc. possible
- **Trust boundary**: Safe code must trust unsafe implementations

### Neutral

- Same model as Rust: unsafe is an escape hatch, not a disable switch
- Standard library wraps syscalls in safe APIs
- Users rarely need to write unsafe directly

## Example: Safe Wrapper

```lisp
; Internal — unsafe
(defun print (x)
  (unsafe
    (let ((buf (to-bytes x)))
      (syscall SYS_WRITE STDOUT buf (len buf)))))

; User code — safe
(print "hello")    ; no unsafe needed
```
