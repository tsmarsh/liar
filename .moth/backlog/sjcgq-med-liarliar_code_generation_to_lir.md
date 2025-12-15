# liarliar: Code Generation to lIR

Create `lib/liarliar/codegen.liar` - emit lIR S-expressions as strings.

**Priority:** HIGH (final compilation pass)

## Related ADRs

- [ADR 019: lIR as Universal Backend](../../doc/adr/019-lir-universal-backend.md) — All frontends emit lIR
- [ADR 020: Toolchain Architecture](../../doc/adr/020-toolchain-architecture.md) — liar → lIR → LLVM pipeline
- [ADR 024: Type-Directed Arithmetic Codegen](../../doc/adr/024-type-directed-arithmetic-codegen.md) — Use types to choose emission strategy

## Overview

Walk the AST (tagged Cons cells) and emit corresponding lIR code using StringBuilder. Per ADR 019, this is the single backend — all liar code goes through lIR.

## Expression Codegen

```clojure
(defun codegen-expr (ctx sb form)
  (cond
    ((int? form)
     (sb-append sb "(i64 ")
     (sb-append-int sb (int-value form))
     (sb-append sb ")"))

    ((symbol? form)
     (sb-append sb "%")
     (sb-append sb (mangle (symbol-name form))))

    ((add? form)
     (sb-append sb "(add ")
     (codegen-expr ctx sb (cadr form))
     (sb-append sb " ")
     (codegen-expr ctx sb (caddr form))
     (sb-append sb ")"))

    ((if? form)
     (codegen-if ctx sb form))

    ((let? form)
     (codegen-let ctx sb form))

    ((call? form)
     (codegen-call ctx sb form))

    ;; ... more forms
    ))
```

## Function Codegen

```clojure
(defun codegen-defun (ctx sb form)
  (sb-append sb "(define ")
  (sb-append sb (return-type form))
  (sb-append sb " @")
  (sb-append sb (defun-name form))
  (sb-append sb " (")
  (codegen-params ctx sb form)
  (sb-append sb ") (block entry ")
  (codegen-expr ctx sb (defun-body form))
  (sb-append sb "))\n"))
```

## Builtin Codegen

Map liar builtins to lIR:
- `+` -> `add`
- `-` -> `sub`
- `*` -> `mul`
- `/` -> `sdiv`
- `=` -> `icmp eq`
- `<` -> `icmp slt`
- etc.

## Functions to Implement

- `codegen-expr` - Main expression dispatcher
- `codegen-defun` - Function definitions
- `codegen-if` - Conditional with blocks
- `codegen-let` - Let bindings
- `codegen-call` - Function calls
- `codegen-builtin` - Builtin operations
- `codegen-closure` - Closure literals
- `mangle` - Name mangling for lIR

## Dependencies

- `lib/liarliar/strings.liar` - StringBuilder
- `lib/liarliar/value.liar` - Tagged value predicates

## Milestones

1. `(defun main () (+ 1 2))` - Integers and arithmetic
2. `(defun square (x) (* x x))` - Function params
3. `(if (< x 0) (- 0 x) x)` - Conditionals
4. Closures and higher-order functions
5. Self-compile liarliar

## Ordering

Depends on: `strings.liar`, `value.liar`, `infer.liar`, `closures.liar`
Required by: `main.liar`

## Design Notes

Per ADR 024, codegen should be type-directed:
- Known `i64` arithmetic → emit lIR `add`, `sub`, etc.
- Known concrete type → emit direct call to type's implementation
- Unknown/polymorphic → emit protocol dispatch

The type information flows from `infer.liar`. When types are concrete, we get optimal code. When types are polymorphic, we pay for dispatch.

Name mangling: liar names like `my-func?` must become valid lIR identifiers. Convention: `?` → `_Q`, `-` → `_`, etc.

Closure codegen emits:
1. Environment struct definition (`defstruct`)
2. Lifted function with env parameter (`define`)
3. Closure construction at use site (`share` + struct literal)
