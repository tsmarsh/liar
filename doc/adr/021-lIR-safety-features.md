# ADR-021: lIR Safety Features

## Status
Proposed

## Context
lIR currently mirrors LLVM IR closely — it's a low-level assembly with S-expression syntax. Memory safety is the frontend's responsibility. This creates problems:

1. **Duplicated effort** — Every frontend must implement borrow checking
2. **No verification** — lIR trusts frontends; bugs in frontend = unsafe code
3. **Limited reuse** — Safety logic can't be shared across languages
4. **Encoding overhead** — Closures require struct+function pattern, enums require tag+union

Rust's MIR demonstrates that safety checking works well at an IR level. The IR is simpler than surface syntax, control flow is explicit, and analysis is more tractable.

## Decision
Extend lIR with five safety features:

1. **Ownership tracking and borrow checking**
2. **Reference counting**
3. **Native closures**
4. **Guaranteed tail calls**
5. **Bounds-checked arrays**

These features are **opt-in**. Existing unsafe lIR remains valid. Safe and unsafe code can interoperate with explicit boundaries.

## Safety Model

### Safe by Default, Unsafe Opt-in
```lisp
; Safe function (default) — ownership checked
(define (safe i64) ((own i64 x))
  ...)

; Unsafe function — no checking, raw pointers allowed
(define unsafe (dangerous i64) ((ptr p))
  ...)

; Safe function calling unsafe code
(define (wrapper i64) ((own ptr p))
  (block entry
    (unsafe                         ; explicit unsafe block
      (ret (call @dangerous p)))))
```

### The Two Worlds
| Safe lIR | Unsafe lIR |
|----------|------------|
| `own T` | `ptr` |
| `ref T`, `refmut T` | raw load/store |
| `rc T` | manual refcount |
| `array T N` with `index` | `getelementptr` |
| `closure` | struct + function |
| `tailcall` | regular call (may not TCO) |

## Feature 1: Ownership and Borrowing

### Pointer Types
```lisp
own T       ; Owned pointer — value dropped when pointer dies
ref T       ; Shared borrow — read-only, lifetime-bound
refmut T    ; Mutable borrow — exclusive, lifetime-bound
```

### Ownership Transfer
```lisp
(define (consume void) ((own i64 x))    ; takes ownership
  (block entry
    (use x)
    (ret)))                              ; x dropped here

(define (borrow i64) ((ref i64 x))      ; borrows
  (block entry
    (ret (load i64 x))))                 ; x not dropped

(define (mutate void) ((refmut i64 x))  ; mutable borrow
  (block entry
    (store (i64 42) x)
    (ret)))                              ; x not dropped
```

### Move Semantics
```lisp
(define (move-example void) ((own i64 x))
  (block entry
    (let ((y x))                         ; x moved to y
      (use y)                            ; OK
      ; (use x)                          ; ERROR: x moved
      (ret))))
```

### Borrow Rules
```lisp
; Multiple shared borrows OK
(let ((a (borrow ref x))
      (b (borrow ref x)))               ; OK
  ...)

; Mutable borrow is exclusive
(let ((a (borrow refmut x))
      (b (borrow ref x)))               ; ERROR: x already mutably borrowed
  ...)

; Borrow cannot outlive owner
(define (escape-error own i64) ()
  (block entry
    (let ((x (alloc i64)))
      (ret (borrow ref x)))))           ; ERROR: x dies, borrow escapes
```

### Explicit Drop
```lisp
(let ((x (alloc own i64)))
  (store (i64 42) x)
  (drop x)                               ; explicit drop
  ; (use x)                              ; ERROR: x dropped
  (ret))
```

### Verification
The lIR verifier checks:
- No use after move
- No use after drop
- Borrows don't outlive owners
- Mutable borrows are exclusive
- All owned values dropped exactly once

## Feature 2: Reference Counting

### RC Pointer Type
```lisp
rc T        ; Reference-counted pointer
```

### Implicit Reference Counting
```lisp
(define (rc-example void) ()
  (block entry
    (let ((x (rc-alloc i64)))           ; refcount = 1
      (store (i64 42) x)
      (let ((y x))                       ; refcount = 2 (implicit clone)
        (use y))                         ; refcount = 1 (y scope ends)
      (ret))))                           ; refcount = 0, freed
```

### RC Operations
```lisp
(rc-alloc T)           ; Allocate with refcount 1
(rc-clone x)           ; Increment refcount, return alias
(rc-drop x)            ; Decrement refcount, free if zero
(rc-count x)           ; Get current refcount (for debugging)
```

### RC is Immutable by Default
```lisp
(let ((x (rc-alloc i64)))
  (store (i64 42) x))                   ; ERROR: rc is immutable

; Use atomic cell for mutable shared state
(let ((x (rc-alloc (atom i64))))
  (atom-store x (i64 42)))              ; OK: atomic mutation
```

### Weak References (Optional)
```lisp
weak T      ; Weak reference (doesn't prevent deallocation)

(let ((strong (rc-alloc i64)))
  (let ((w (rc-downgrade strong)))      ; weak ref
    (match (rc-upgrade w)               ; try to get strong ref
      ((some s) (use s))
      (none (handle-gone)))))
```

## Feature 3: Native Closures

### Closure Syntax
```lisp
(closure NAME ((CAPTURE-MODE TYPE NAME) ...)
  (fn RETURN-TYPE ((TYPE PARAM) ...)
    BODY))
```

### Capture Modes
```lisp
; Move capture — closure owns the value
(closure add-n ((own i64 n))
  (fn i64 ((i64 x))
    (block entry
      (ret (add n x)))))

; Borrow capture — closure borrows, cannot escape
(closure peek ((ref i64 data))
  (fn i64 ()
    (block entry
      (ret (load i64 data)))))

; RC capture — closure shares ownership
(closure shared ((rc i64 data))
  (fn i64 ()
    (block entry
      (ret (load i64 data)))))
```

### Closure Type
```lisp
closuretype NAME RETURN-TYPE (PARAM-TYPES...)

; Example
(closuretype IntToInt i64 (i64))

(define (apply i64) ((own IntToInt f) (i64 x))
  (block entry
    (ret (closure-call f x))))
```

### Closure Operations
```lisp
(closure-call f args...)    ; Call closure
(closure-env f)             ; Get environment pointer (unsafe)
(closure-fn f)              ; Get function pointer (unsafe)
```

### Closure Color
Closures track thread-safety:
```lisp
; Pure — no captures, can go anywhere
(closure pure ()
  (fn i64 ((i64 x))
    (block entry (ret (add x (i64 1))))))

; Sync — captures only Send+Sync, thread-safe
(closure sync ((rc i64 data))
  ...)

; Local — captures borrows, cannot escape scope
(closure local ((ref i64 data))
  ...)
```

## Feature 4: Guaranteed Tail Calls

### Tailcall Instruction
```lisp
(tailcall @function args...)
```

### Semantics
- Reuses current stack frame
- **Guaranteed** — compiler error if TCO not possible
- Enables efficient recursion

### Examples
```lisp
; Tail-recursive factorial
(define (factorial-tail i64) ((i64 n) (i64 acc))
  (block entry
    (br (icmp eq n (i64 0)) done recurse))
  (block done
    (ret acc))
  (block recurse
    (tailcall @factorial-tail (sub n (i64 1)) (mul n acc))))

; Mutual tail recursion
(define (even? i1) ((i64 n))
  (block entry
    (br (icmp eq n (i64 0)) yes no))
  (block yes
    (ret (i1 1)))
  (block no
    (tailcall @odd? (sub n (i64 1)))))

(define (odd? i1) ((i64 n))
  (block entry
    (br (icmp eq n (i64 0)) no yes))
  (block no
    (ret (i1 0)))
  (block yes
    (tailcall @even? (sub n (i64 1)))))
```

### When Tailcall Fails
Tailcall is rejected if:
- Not in tail position
- Return type mismatch
- Caller has cleanup (drops) after call

```lisp
; ERROR: not in tail position
(define (bad i64) ((i64 n))
  (block entry
    (let ((result (tailcall @foo n)))   ; ERROR
      (ret (add result (i64 1))))))

; ERROR: needs cleanup
(define (bad2 i64) ((own ptr p) (i64 n))
  (block entry
    (tailcall @foo n)))                  ; ERROR: p needs drop
```

## Feature 5: Bounds-Checked Arrays

### Array Type
```lisp
array T N   ; Fixed-size array of N elements of type T
slice T     ; Dynamically-sized view into array
```

### Array Operations
```lisp
(array-alloc T N)                ; Allocate array
(array-get arr idx)              ; Bounds-checked read
(array-set arr idx val)          ; Bounds-checked write
(array-len arr)                  ; Get length

; Slicing
(array-slice arr start end)      ; Create slice
(slice-get s idx)                ; Bounds-checked read
(slice-len s)                    ; Get length
```

### Bounds Checking
```lisp
(define (safe-access i64) ((ref (array i64 10) arr) (i64 idx))
  (block entry
    (ret (array-get arr idx))))          ; Panics if idx >= 10
```

### Unchecked Access (Unsafe)
```lisp
(define unsafe (fast-access i64) ((ptr arr) (i64 idx))
  (block entry
    (ret (load i64 (getelementptr i64 arr idx)))))
```

### Static Bounds Elimination
When index is constant and provably in-bounds:
```lisp
(define (static-safe i64) ((ref (array i64 10) arr))
  (block entry
    (ret (array-get arr (i64 5)))))      ; No runtime check (5 < 10)
```

## Interop: Safe and Unsafe

### Safe Calling Unsafe
```lisp
(define (wrapper i64) ((own ptr p))
  (block entry
    (let ((result 
           (unsafe                       ; explicit unsafe block
             (call @dangerous_c_function p))))
      (ret result))))
```

### Unsafe Calling Safe
Unsafe code can call safe code normally — safe code maintains its invariants.

### FFI Boundary
External declarations are implicitly unsafe:
```lisp
(declare malloc ptr (i64))               ; unsafe
(declare free void (ptr))                ; unsafe
(declare printf i32 (ptr) ...)           ; unsafe

(define (safe-printf void) ((ref (array i8 100) fmt))
  (block entry
    (unsafe
      (call @printf (array-ptr fmt)))    ; explicit unsafe for FFI
    (ret)))
```

## Implementation Order

1. **Tail calls** — Simplest, isolated feature
2. **Bounds-checked arrays** — Relatively simple, high value
3. **Native closures** — Complex but well-understood pattern
4. **Reference counting** — Requires careful codegen
5. **Ownership and borrowing** — Most complex, requires verifier

## Consequences

### Positive
- Safety verified at IR level — single source of truth
- Multiple frontends benefit from same safety infrastructure
- Cleaner codegen — no closure/enum encoding tricks
- Guaranteed TCO enables functional patterns
- Bounds checking prevents buffer overflows

### Negative
- lIR becomes more complex — larger spec, more to learn
- Verifier adds compile-time overhead
- Some LLVM optimizations may be hindered by safety wrappers
- Migration path needed for existing lIR code

### Neutral
- Unsafe escape hatch preserves full power when needed
- Existing unsafe lIR remains valid (backward compatible)

## References
- Rust MIR: https://rustc-dev-guide.rust-lang.org/mir/
- ADR-001 through ADR-014: Ownership model for liar
- ADR-010: Closure color tracking