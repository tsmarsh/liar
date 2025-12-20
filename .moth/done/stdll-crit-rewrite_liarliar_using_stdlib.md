# Rewrite liarliar Using Standard Library

## Summary

Rewrite the liarliar self-hosted compiler to use liar's standard library (`liar.seq`, `liar.hashmap`, `liar.core`) instead of reimplementing everything from scratch. The goal is not just a simpler compiler, but to **validate that liar's abstractions actually work** for real code.

## Motivation

The current liarliar is ~2,000 lines that carefully avoids using liar's stdlib:

| Current liarliar | Stdlib equivalent |
|------------------|-------------------|
| `value.liar` — ICons, PCons, SCons | `liar.seq` — Cons, protocols |
| `symbols.liar` — hand-rolled hash table | `liar.hashmap` — HashMap |
| `strings.liar` — StringBuilder | `liar.core` or `liar.io` |
| `env.liar` — manual assoc list | `liar.hashmap` — HashMap |
| Manual `car`/`cdr`/`cadr` chains | `first`, `second`, `third`, `rest` |
| `(pcons ctx result)` threading | `->` threading macro |

This parallel implementation:
1. **Doesn't validate the stdlib** — If HashMap is broken, we'd never know
2. **Is unnecessarily verbose** — 2,000 lines instead of ~500
3. **Defeats the purpose of bootstrap** — We're not proving liar works, just that we can emit lIR

The whole point of self-hosting is: *if liarliar works, liar works*. But that's only true if liarliar actually uses liar.

## Philosophy

**When liarliar hits a wall, the fix belongs in liar, not in a workaround.**

Examples:
- HashMap doesn't work with string keys? Fix HashMap, don't hand-roll a symbol table.
- Protocols don't dispatch correctly? Fix protocol dispatch, don't use `instance?` chains.
- Threading macro doesn't expand right? Fix the macro, don't manually thread state.

Each failure is a gift — it tells us exactly what's broken in liar.

## Target Architecture

### Reader (reader.liar)

```lisp
(ns liarliar.reader
  (:require [liar.core :refer :all]
            [liar.seq :refer :all]))

;; Use stdlib Cons, not hand-rolled SCons
(defstruct SourceLoc (line: i64 col: i64))
(defstruct Located (value: ptr loc: ptr))

(defun read-list (lex)
  (let ((tok (peek-token lex)))
    (if (= tok ")")
        nil
        (cons (read-expr lex) (read-list lex)))))

;; Use first/rest, not car/cdr
(defun parse-defun (form)
  (let ((name (second form))
        (params (third form))
        (body (fourth form)))
    ...))
```

### Symbol Table (symbols.liar)

```lisp
(ns liarliar.symbols
  (:require [liar.hashmap :refer :all]))

;; Use HashMap, not hand-rolled table
(defun make-symbol-table ()
  (hashmap))

(defun intern (table name)
  (let ((existing (get table name)))
    (if (nil? existing)
        (let ((id (count table))
              (sym (make-symbol name id)))
          (values (assoc table name sym) sym))
        (values table existing))))
```

### Codegen Context

```lisp
(ns liarliar.codegen
  (:require [liar.core :refer :all]
            [liar.seq :refer :all]))

;; Use -> threading instead of manual pcons unpacking
(defun codegen-binop (ctx form op)
  (-> ctx
      (codegen-expr (second form))
      (bind-result left)
      (codegen-expr (third form))
      (bind-result right)
      (emit (lir-binop op left right))))

;; Or if we need explicit state, use a clean pattern
(defun codegen-expr (ctx form)
  (match (classify form)
    :int    (codegen-int ctx form)
    :symbol (codegen-symbol ctx form)
    :list   (codegen-list ctx form)))
```

### Environment

```lisp
(ns liarliar.env
  (:require [liar.hashmap :refer :all]))

;; Environments are just hashmaps with a parent pointer
(defstruct Env (bindings: ptr parent: ptr))

(defun env-lookup (env name)
  (let ((local (get (. env bindings) name)))
    (if (nil? local)
        (if (nil? (. env parent))
            nil
            (env-lookup (. env parent) name))
        local)))

(defun env-define (env name value)
  (Env (assoc (. env bindings) name value) (. env parent)))
```

## Implementation Plan

### Phase 1: Identify Dependencies

Before rewriting, audit what liarliar actually needs:

1. **Sequences** — cons, first, rest, map, filter (from `liar.seq`)
2. **Hash maps** — for symbol table, environments (from `liar.hashmap`)
3. **Strings** — comparison, building (from `liar.core` or builtins)
4. **I/O** — file reading (from `liar.io`)
5. **Macros** — quasiquote, threading (from `liar.core`)

Write a minimal test for each:

```lisp
;; test-stdlib-deps.liar
(ns test-stdlib-deps
  (:require [liar.seq :refer :all]
            [liar.hashmap :refer :all]))

(defun test-cons ()
  (let ((lst (cons 1 (cons 2 nil))))
    (assert-eq (first lst) 1)
    (assert-eq (first (rest lst)) 2)))

(defun test-hashmap ()
  (let ((m (-> (hashmap) (assoc "foo" 42))))
    (assert-eq (get m "foo") 42)))

;; ... etc
```

**If these tests fail, fix liar first.**

### Phase 2: Rewrite Reader

Replace `reader.liar` + `value.liar` with a version using `liar.seq`:

- Use `Cons` instead of `SCons`
- Use `first`/`rest` instead of `car`/`cdr`
- Keep `SourceLoc` if needed for error messages, but attach as metadata or wrapper

Target: ~150 lines (down from ~600)

### Phase 3: Rewrite Symbol Table

Replace `symbols.liar` with HashMap-based implementation:

- Use `liar.hashmap` for storage
- Symbol identity via map lookup, not manual ID tracking

Target: ~30 lines (down from ~80)

### Phase 4: Rewrite Environments

Replace `env.liar` with HashMap + parent chain:

Target: ~40 lines (down from ~150)

### Phase 5: Rewrite Codegen

Replace `codegen.liar` with cleaner version:

- Use `->` threading where applicable
- Use `first`/`second`/`third` instead of `car`/`cadr`/`caddr`
- Use pattern matching if available, or clean cond dispatch

Target: ~200 lines (down from ~400)

### Phase 6: Simplify Printer

Replace `strings.liar` + `printer.liar`:

- Use stdlib string operations if available
- Or keep minimal StringBuilder if truly needed

Target: ~80 lines (down from ~350)

### Phase 7: Simplify Macro Expander

The current `expand.liar` may be fine, but review for stdlib usage opportunities.

Target: ~150 lines (down from ~250)

## Expected Outcome

| Component | Current | Target | Reduction |
|-----------|---------|--------|-----------|
| reader.liar | 450 | 150 | 67% |
| value.liar | 150 | 0 (use liar.seq) | 100% |
| symbols.liar | 80 | 30 | 63% |
| strings.liar | 200 | 0 (use stdlib) | 100% |
| printer.liar | 150 | 80 | 47% |
| codegen.liar | 400 | 200 | 50% |
| expand.liar | 250 | 150 | 40% |
| env.liar | 150 | 40 | 73% |
| io.liar | 50 | 0 (use liar.io) | 100% |
| main.liar | 80 | 50 | 38% |
| **Total** | **~2000** | **~700** | **65%** |

## Bug Discovery Protocol

When something doesn't work:

1. **Write a minimal reproduction** — Smallest liar program that fails
2. **Add to cert tests** — Create a new scenario in `liar-cert/features/`
3. **Fix in liar** — The Rust compiler, stdlib, or codegen
4. **Verify fix** — Cert test passes
5. **Continue liarliar rewrite** — Now that the bug is fixed

This turns liarliar into a **systematic fuzzer for liar's stdlib**.

## Bugs We Might Find

Based on the workarounds in current liarliar, likely issues include:

- **HashMap with string keys** — May not hash/compare correctly
- **Protocol dispatch on nil** — Edge case handling
- **Closures capturing mutable state** — The StringBuilder pattern
- **String literals as ptr** — Type mismatch between `string` and `ptr`
- **Threading macro expansion** — May not handle all forms
- **Struct field access in protocols** — Self parameter handling

Each of these, if found, is valuable — it's a real bug we'd hit in production.

## Acceptance Criteria

- [ ] `test-stdlib-deps.liar` passes (proves stdlib works for our needs)
- [ ] liarliar rewritten to use `liar.seq`, `liar.hashmap`, `liar.core`
- [ ] Total line count under 800
- [ ] No hand-rolled data structures (no ICons/PCons/SCons, no manual hash table)
- [ ] Uses `first`/`rest` not `car`/`cdr`
- [ ] All existing liarliar tests still pass
- [ ] Any bugs found are fixed in liar with cert test coverage

## Relationship to Other Moths

**Depends on:**
- `001-br4nc` (branching) — Recursion must work for any real code

**Enables:**
- Confidence that liar's stdlib is production-ready
- Faster iteration on liarliar features
- Cleaner path to full self-hosting

## Success Metric

**liarliar is a validation of liar, not a workaround for it.**

If we finish this rewrite and liarliar works, we've proven:
- `liar.seq` works for real recursive data processing
- `liar.hashmap` works for real lookup-heavy code
- Protocols dispatch correctly in real usage
- Macros expand correctly in real usage
- liar is ready for production use

That's worth far more than a 2,000-line bootstrap that avoids testing anything.
