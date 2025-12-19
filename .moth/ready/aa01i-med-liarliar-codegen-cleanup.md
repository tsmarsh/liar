# Liarliar Codegen Cleanup

## Summary

Clean up `liarliar/codegen.liar` to use stdlib patterns after other rewrites are done.

**This should be done LAST** after reader, symbols, env are rewritten.

## Current State

`codegen.liar` (~2200 lines) is the largest file:
- Manual `car`/`cdr`/`cadr` chains
- `pcons` pattern for context threading
- Hand-coded symbol helpers
- Lots of helper functions

## Target State

~800-1000 lines by:
1. Using `first`/`second`/`third`/`rest`
2. Using `->` threading macro where applicable
3. Removing redundant helpers
4. Leveraging cleaner stdlib patterns

## Changes

### 1. Replace car/cdr chains

Before:
```lisp
(let ((head (car form))
      (name (cadr form))
      (params (caddr form))
      (body (cadddr form)))
  ...)
```

After:
```lisp
(let ((head (first form))
      (name (second form))
      (params (third form))
      (body (fourth form)))
  ...)
```

### 2. Consider threading macro

Before:
```lisp
(let ((result1 (codegen-expr ctx (second form)))
      (ctx1 (pcons-head result1))
      (left (pcons-tail result1))
      (result2 (codegen-expr ctx1 (third form)))
      (ctx2 (pcons-head result2))
      (right (pcons-tail result2)))
  (pcons ctx2 (lir-binop op left right)))
```

After (if threading works well):
```lisp
(-> ctx
    (codegen-with-result (second form))
    (bind left)
    (codegen-with-result (third form))
    (bind right)
    (emit (lir-binop op left right)))
```

Or keep explicit but cleaner:
```lisp
(let-values (((ctx left) (codegen-expr ctx (second form)))
             ((ctx right) (codegen-expr ctx (third form))))
  (pcons ctx (lir-binop op left right)))
```

### 3. Remove dead helpers

Audit and remove any unused:
- String builder helpers (if printer handles this)
- Redundant symbol constructors
- One-off helpers that could be inlined

### 4. Consolidate similar patterns

Many `codegen-*` functions follow the same pattern. Consider:
- Macro for common patterns
- Higher-order function for dispatch

## Testing

After each change:
1. Rebuild liarliar
2. Run: `USE_LIARLIAR=1 cargo test --package liar-spec`
3. Test edge cases (closures, macros, protocols)

## Depends On

- `gr4zj` (reader) - must use new Cons/first/rest interface
- `f75jg` (symbols) - may affect symbol helpers
- `fdrg3` (env) - may affect env helpers

## Acceptance Criteria

- [ ] No `car`/`cdr` anywhere
- [ ] Uses `first`/`rest`/`second`/`third`
- [ ] Threading macro used where it improves readability
- [ ] All liarliar tests pass
- [ ] Line count < 1200 (down from ~2200)
