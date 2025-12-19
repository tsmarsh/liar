# Macros And Quasiquote In Liarliar

## Problem

The `expand.liar` module exists but quasiquote expansion is fundamentally broken. It produces runtime list-building code instead of compile-time template substitution.

**Current (wrong):** `(defmacro add-one (x) \`(+ ,x 1))` produces:
```lisp
(cons (quote +) (cons 5 (cons 1 nil)))
```

**Expected:** Should produce the actual code structure:
```lisp
(+ 5 1)
```

## Root Cause

The `expand-qq` function in `expand.liar:257` treats quasiquote as a runtime operation (generating `cons`/`quote` calls) rather than a compile-time template mechanism.

## Plan

1. Rewrite `expand-qq` to perform compile-time S-expression construction:
   - Atoms without unquote -> return as-is (not wrapped in quote)
   - `~x` (unquote) -> substitute the bound value directly
   - `~@xs` (unquote-splicing) -> splice values into the list at compile time
   - Lists -> recursively build scons structures

2. Remove `make-cons-form`, `make-append-form`, `make-quote-form` helpers (they're for runtime code generation)

3. Test against `liar-cert/features/macros.feature` (9 scenarios)

## Test Command

```bash
USE_LIARLIAR=1 cargo test --release -p liar-cert --test cert 2>&1 | grep -A 30 "Feature: Macros"
```

## Files to Modify

- `liarliar/expand.liar` - Fix quasiquote expansion
- `liarliar/README.md` - Update limitations section when done
