# Liarliar Reader Seq

## Summary

Rewrite `liarliar/reader.liar` and `liarliar/value.liar` to use `liar.seq` instead of hand-rolled SCons.

**This is the largest and riskiest rewrite** because SCons is used throughout liarliar.

## Current State

- `value.liar` (~180 lines) - defines ICons, PCons, SCons, BoxedInt, BoxedBool, etc.
- `reader.liar` (~590 lines) - lexer and parser producing SCons trees

## Target State

- `value.liar` (~50 lines) - keep BoxedInt, BoxedBool, BoxedFloat, LiarString only
- `reader.liar` (~150 lines) - use `liar.seq` Cons for parsed lists

## The Challenge

SCons is used everywhere:
- Reader produces SCons
- Codegen consumes SCons
- Printer traverses SCons
- Expand transforms SCons

Changing to `Cons` from `liar.seq` requires:
1. Update reader to produce Cons
2. Update codegen to consume Cons (use `first`/`rest`)
3. Update printer to handle Cons
4. Update expand to use Cons

## Strategy

### Option A: Big Bang
Replace SCons with Cons everywhere at once. High risk, but cleaner.

### Option B: Compatibility Layer
Keep SCons but implement it as a wrapper around Cons. Lower risk.

### Recommended: Option A with careful testing

Since the interfaces (`first`, `rest`, `cons`) are the same, Option A is feasible:

```lisp
;; Before (value.liar)
(defstruct SCons (scons-hd: ptr scons-tl: ptr scons-line: i64 scons-col: i64))
(defun car (lst) (if (scons? lst) (. lst scons-hd) nil))
(defun cdr (lst) (if (scons? lst) (. lst scons-tl) nil))

;; After (using liar.seq)
;; car -> first
;; cdr -> rest
;; (scons x y) -> (cons x y)
```

## Source Location Handling

Current SCons stores line/col. Options:
1. **Drop it** - We rarely use it anyway
2. **Wrapper struct** - `(Located value line col)` wraps any value
3. **Metadata** - If liar supports metadata on values

Recommend: Start with option 1 (drop). Add back if needed.

## Migration Steps

1. Update `value.liar`:
   - Remove SCons definition
   - Keep ICons, PCons for non-AST uses
   - Add `(:require [liar.seq :refer :all])`

2. Update `reader.liar`:
   - Replace `(scons x y)` with `(cons x y)`
   - Replace `scons?` with `cons?` or just check non-nil

3. Search-replace in all files:
   - `scons-head` -> `first`
   - `scons-tail` -> `rest`
   - `car` -> `first`
   - `cdr` -> `rest`
   - `cadr` -> `second`
   - `caddr` -> `third`
   - `cadddr` -> `fourth`

4. Update `printer.liar`:
   - Handle `Cons` from liar.seq instead of SCons

## Testing

After each step:
1. Rebuild liarliar
2. Test: `/tmp/liarliar /tmp/test.liar`
3. Run: `USE_LIARLIAR=1 cargo test --package liar-spec`

## Depends On

- `n02qm` (stdlib deps) - seq must work
- Ideally after `jsoyp` (printer) since printer needs updating

## Blocks

- `aa01i` (codegen cleanup) - should wait for this

## Acceptance Criteria

- [ ] value.liar has no SCons
- [ ] reader.liar uses `liar.seq` Cons
- [ ] No `car`/`cdr` anywhere (use `first`/`rest`)
- [ ] All liarliar tests pass
- [ ] reader.liar + value.liar < 200 lines total
