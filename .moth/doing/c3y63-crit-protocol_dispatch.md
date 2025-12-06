# Protocol Dispatch

**Priority:** P0 (Critical - blocks async I/O)
**Category:** liar/codegen
**Dependencies:** Struct Instantiation (u1dv9)

## Summary

Implement runtime dispatch for protocol methods. Currently `defprotocol` and `extend-protocol` are parsed but generate no code.

## Current State

In `liar/src/codegen.rs`:
- `Item::Defprotocol` returns `Ok(None)` - skipped
- `Item::ExtendProtocol` returns `Ok(None)` - skipped

Protocol method calls are not recognized and would fail.

## Requirements

### defprotocol
- Defines a protocol with method signatures
- Creates dispatch table structure

### extend-protocol
- Implements protocol methods for a specific type
- Registers implementation in dispatch table

### Method Dispatch
- Protocol method calls resolve at runtime based on receiver type
- `(method obj args...)` looks up `obj`'s type and dispatches

## Implementation

**Location:** `liar/src/codegen.rs`, possibly new `liar/src/protocols.rs`

### Approach 1: Static Dispatch (simpler, less flexible)
- At compile time, resolve protocol method to concrete function
- Requires knowing concrete type at call site
- Generate direct function calls

### Approach 2: Dynamic Dispatch (more flexible)
- Generate vtable-style dispatch tables
- Each type's implementation stored in table
- Runtime lookup based on type tag

### For async I/O, we need at minimum:
```lisp
(defprotocol Pollable
  (poll [self waker]))

(defstruct ImmediateFuture (value: i64))

(extend-protocol Pollable ImmediateFuture
  (poll [self waker] (.value self)))

;; This must work:
(let ((f (ImmediateFuture 42)))
  (poll f nil))  ;; => 42
```

### lIR Generation (dynamic approach):
```lisp
;; defprotocol creates:
;; - Protocol ID constant
;; - Dispatch function that takes (type-id, self, args...)

;; extend-protocol creates:
;; - Concrete implementation function
;; - Registration in dispatch table

;; Method call (poll f nil) becomes:
(call __dispatch_poll (type-of f) f nil)
```

## Tests

```lisp
;; Basic protocol
(defprotocol Greet
  (greet [self]))

(defstruct Person (name: String))
(defstruct Dog (name: String))

(extend-protocol Greet Person
  (greet [self] (str "Hello, " (.name self))))

(extend-protocol Greet Dog
  (greet [self] (str "Woof! I'm " (.name self))))

(greet (Person "Alice"))  ;; => "Hello, Alice"
(greet (Dog "Buddy"))     ;; => "Woof! I'm Buddy"

;; Multi-method protocol
(defprotocol Counted
  (count [self]))

(extend-protocol Counted PersistentVector
  (count [self] (vec-len self)))

;; Protocol with multiple args
(defprotocol Addable
  (add [self other]))
```

## Acceptance Criteria

- [ ] `defprotocol` creates protocol metadata
- [ ] `extend-protocol` registers implementation
- [ ] `(extend-protocol P T (m [self] 42))` then `(m (T))` returns `42`
- [ ] Multiple types can implement same protocol
- [ ] Protocol methods with multiple args work
- [ ] Tests added to liar-cert
