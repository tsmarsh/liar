# ADR 024: Type-Directed Arithmetic Codegen

## Status

Proposed

## Context

Arithmetic operations (`+`, `-`, `*`, `/`, etc.) need to work with multiple numeric types: primitive integers, floats, rationals, bignums, and potentially user-defined numeric types. The naive approach of always emitting protocol dispatch would sacrifice performance for simple integer arithmetic.

Currently, `(+ x 1)` compiles directly to lIR's `add` instruction, which only works for primitive integers. This breaks when `x` is a rational or other numeric type.

## Decision

Arithmetic operators are protocols, but codegen is **type-directed**. The compiler emits different code based on what it knows at compile time:

### 1. Known Primitive Type → Emit Primitive Instruction

When both operands have known primitive types, emit the lIR instruction directly:

```lisp
;; liar
(defun inc (x: i64) (+ x 1))

;; lIR output
(define (inc i64) ((i64 x))
  (block entry (ret (add x (i64 1)))))
```

No protocol overhead. LLVM sees a simple `add`.

### 2. Known Concrete Type → Emit Direct Call

When the concrete type is known but not primitive, emit a direct call to that type's implementation:

```lisp
;; liar
(defun inc-rational (x: Rational) (+ x 1))

;; lIR output
(define (inc-rational ptr) ((ptr x))
  (block entry (ret (call @rational_add x (call @rational_from_i64 (i64 1))))))
```

Direct call, no dispatch overhead.

### 3. Unknown/Polymorphic Type → Emit Protocol Dispatch

When the type is unknown or the function is explicitly polymorphic, emit full dispatch:

```lisp
;; liar
(defun inc (x) (+ x 1))  ; x could be anything Addable

;; lIR output (conceptual)
(define (inc ptr) ((ptr x))
  (block entry
    (ret (call @addable_dispatch x (call @box_i64 (i64 1))))))
```

The dispatch function checks type tags and routes to the appropriate implementation.

### Type Inference Propagation

The key enabler is type inference that flows through:

- Literal types: `5` is `i64`, `5.0` is `f64`, `5/4` is `Rational`
- Function parameter annotations: `(x: i64)` constrains `x`
- Return type annotations: `-> i64` constrains the body
- Call site context: `(inc 5)` tells us `x` receives an `i64`
- Let bindings: `(let ((x 5)) ...)` knows `x: i64`

### Protocol Definition

Arithmetic protocols define the interface:

```lisp
(defprotocol Addable
  (add [self other]))

(defprotocol Subtractable
  (sub [self other]))

;; etc.
```

Primitive types have implicit protocol implementations that the compiler recognizes and optimizes away.

## Consequences

### Positive

- **Zero-cost abstraction**: Primitives compile to primitives
- **Extensible**: New numeric types just implement protocols
- **Gradual typing**: Untyped code works (with dispatch cost), typed code is fast
- **No special-casing in lIR**: All abstraction lives in liar

### Negative

- **Compiler complexity**: Type inference must propagate far enough
- **Subtle performance cliffs**: Adding a polymorphic call site can slow down code
- **Inference limitations**: Some patterns may not infer well

### Neutral

- Similar to Rust's trait system with monomorphization
- Similar to Julia's multiple dispatch with JIT specialization
- Follows the project's "all abstraction in liar" principle

## Related

- ADR 015: Numeric Primitives — defines the primitive types
- ADR 017: Type Promotion — defines mixed-type behavior
