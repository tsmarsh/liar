# liarliar: Main Entry Point

Create `lib/liarliar/main.liar` - orchestrate the compiler pipeline.

**Priority:** HIGH (final integration point)

## Related ADRs

- [ADR 019: lIR as Universal Backend](../../doc/adr/019-lir-universal-backend.md) — Output is lIR
- [ADR 020: Toolchain Architecture](../../doc/adr/020-toolchain-architecture.md) — Bootstrap path defined here

## Overview

Wire together all compiler passes. Read file, compile, write lIR output. This is the self-hosted liar compiler.

## Main Function

```clojure
(defun compile-file (path)
  (let ((source (slurp path))
        (data (read-string source))
        (expanded (expand-macros data))
        (resolved (resolve-names expanded))
        (types (infer-types resolved))
        (checked (check-ownership resolved types))
        (converted (convert-closures resolved))
        (lir (codegen converted types)))
    (spit (str path ".lir") lir)))
```

## Entry Point

```clojure
(defun main (argc: i64 argv: ptr) -> i64
  (if (< argc 2)
      (do (println "Usage: liarliar <file.liar>") 1)
      (let ((path (aget argv 1)))
        (do
          (compile-file path)
          (println "Compiled successfully")
          0))))
```

## Compiler Context

```clojure
(defstruct CompilerCtx
  (symbols: ptr     ;; Symbol table
   structs: ptr     ;; Struct definitions
   protocols: ptr   ;; Protocol definitions
   functions: ptr   ;; Function signatures
   macros: ptr      ;; Macro definitions
   type-env: ptr    ;; Type bindings
   errors: ptr      ;; Error accumulator
   fresh-id: i64))  ;; Counter for gensym

(defun make-compiler-ctx ()
  (share (CompilerCtx
          (make-symbol-table)
          (str-map)
          (str-map)
          (str-map)
          (str-map)
          (str-map)
          (vector)
          0)))
```

## Error Handling

```clojure
(defun error! (ctx msg form)
  ;; Accumulate error, continue compilation
  (let ((err {:msg msg :form form})
        (errs (vec-conj (. ctx errors) err)))
    (update-errors ctx errs)))

(defun has-errors? (ctx)
  (> (vec-count (. ctx errors)) 0))

(defun print-errors (ctx)
  (for-each (. ctx errors)
    (fn (e) (println (str "Error: " (. e msg))))))
```

## Functions to Implement

- `main` - CLI entry point
- `compile-file` - Full compilation pipeline
- `compile-string` - For testing
- `make-compiler-ctx` - Initialize context
- `error!` - Accumulate errors
- `print-errors` - Report all errors

## Dependencies

All other liarliar modules:
- `lib/liarliar/value.liar`
- `lib/liarliar/reader.liar`
- `lib/liarliar/symbols.liar`
- `lib/liarliar/expand.liar`
- `lib/liarliar/resolve.liar`
- `lib/liarliar/infer.liar`
- `lib/liarliar/ownership.liar`
- `lib/liarliar/closures.liar`
- `lib/liarliar/codegen.liar`
- `lib/liarliar/strings.liar`

Also:
- `liar.io` - slurp, spit

## Milestones

1. Compile `(defun main () 42)` - Minimal
2. Compile with functions and arithmetic
3. Compile with closures
4. Self-compile liarliar!

## Bootstrap Test

```bash
# Compile liarliar with Rust compiler
liarc lib/liarliar/main.liar -o liarliar1

# Compile liarliar with itself
./liarliar1 lib/liarliar/main.liar
lair lib/liarliar/main.liar.lir -o liarliar2

# Verify identical output
./liarliar1 test.liar > test1.lir
./liarliar2 test.liar > test2.lir
diff test1.lir test2.lir  # Should be identical
```

## Ordering

Depends on: ALL other liarliar modules
This is the final integration point.

## Pipeline Order

Per ADR 020, the compilation pipeline is:

```
source text
    ↓ reader.liar
AST (tagged Cons cells)
    ↓ expand.liar
expanded AST
    ↓ resolve.liar
resolved AST (with binding info)
    ↓ infer.liar
typed AST
    ↓ closures.liar
closure-converted AST
    ↓ ownership.liar (optional for bootstrap)
ownership-checked AST
    ↓ codegen.liar
lIR source text
    ↓ lair (external)
native executable
```

## Design Notes

Error handling: accumulate errors from all passes rather than failing early. This gives users all errors at once, not one at a time.

The compiler context (`CompilerCtx`) threads through all passes, accumulating:
- Symbol table (interned symbols)
- Type environment (inferred types)
- Struct definitions
- Protocol definitions
- Macro definitions
- Errors

For bootstrap, ownership checking can be disabled or simplified. The priority is getting the compiler to compile itself — safety can be added incrementally.
