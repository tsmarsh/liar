# ADR 025: Explicit Arithmetic Names

## Status

Proposed

## Context

Most Lisps use symbolic operators like `+`, `-`, `*`, `/` for arithmetic. These require special handling in the reader (they're valid identifier characters) and create ambiguity about when symbols become operations.

We need to decide: are arithmetic operators special syntax, or just regular symbols?

## Decision

**Core arithmetic uses explicit names: `add`, `sub`, `mul`, `div`, etc.**

The symbols `+`, `-`, `*`, `/` have no special meaning in the core language. They're just symbols like any other.

### Core Primitives

**Arithmetic:**

| Name | Operation |
|------|-----------|
| `add` | Integer addition |
| `sub` | Integer subtraction |
| `mul` | Integer multiplication |
| `div` | Integer division (signed) |
| `mod` | Integer modulo |
| `fadd` | Float addition |
| `fsub` | Float subtraction |
| `fmul` | Float multiplication |
| `fdiv` | Float division |
| `fmod` | Float modulo |

**Comparison:**

| Name | Operation |
|------|-----------|
| `eq` | Equal |
| `neq` | Not equal |
| `lt` | Less than |
| `lte` | Less than or equal |
| `gt` | Greater than |
| `gte` | Greater than or equal |

These map to lIR's `icmp eq`, `icmp slt`, etc.

### Standard Library Sugar

The standard prelude defines convenient aliases:

```lisp
;; In lib/prelude.liar

;; Arithmetic
(defmacro + (a b) `(add ,a ,b))
(defmacro - (a b) `(sub ,a ,b))
(defmacro * (a b) `(mul ,a ,b))
(defmacro / (a b) `(div ,a ,b))
(defmacro % (a b) `(mod ,a ,b))

;; Comparison
(defmacro = (a b) `(eq ,a ,b))
(defmacro != (a b) `(neq ,a ,b))
(defmacro < (a b) `(lt ,a ,b))
(defmacro <= (a b) `(lte ,a ,b))
(defmacro > (a b) `(gt ,a ,b))
(defmacro >= (a b) `(gte ,a ,b))

;; Or with protocol dispatch for polymorphism:
(defun + (a b) (add a b))  ; where add is a protocol method
```

### Pipeline

```
source: (+ 1 2)
    ↓ reader
AST: (+ 1 2)     ; + is just a symbol
    ↓ user macro expansion
AST: (+ 1 2)     ; user macros see +, can transform it
    ↓ standard macro expansion (prelude)
AST: (add 1 2)   ; + expands to add
    ↓ codegen
lIR: (add (i64 1) (i64 2))
```

## Consequences

### Positive

- **Simple reader** — No special cases for operator characters
- **Explicit core** — `add` is unambiguous, greppable
- **lIR alignment** — Core names match the backend
- **User macro visibility** — Macros see original `+` syntax
- **Optional sugar** — Users can skip prelude for explicit style
- **Customizable** — Users can redefine `+` to mean something else

### Negative

- **Verbose core** — `(add 1 2)` vs `(+ 1 2)`
- **Two-phase expansion** — User macros then standard macros
- **Learning curve** — Users expect `+` to "just work"

### Neutral

- Similar to how Clojure has `first`/`rest` as primary, `car`/`cdr` as aliases
- Assembly languages use explicit names (`ADD`, `SUB`)
- The prelude makes the default experience identical to other Lisps

## Related

- ADR 024: Type-Directed Arithmetic Codegen — How `add` becomes primitive or dispatch
- ADR 015: Numeric Primitives — The types involved
- ADR 019: lIR as Universal Backend — Names align with lIR
