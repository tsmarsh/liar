# Liarliar Expand Review

## Summary

Review `liarliar/expand.liar` for stdlib usage opportunities. Lower priority since current expand.liar may be acceptable.

## Current State

`expand.liar` (~250 lines) implements:
- Macro expansion
- Quasiquote/unquote/splice handling
- `gensym` for hygienic macros
- Recursive expansion

## Review Goals

1. **Use `first`/`rest`/`second`/`third`** instead of `car`/`cdr`/`cadr`/`caddr`
2. **Use threading macros** if applicable
3. **Remove dead code** if any
4. **Simplify recursion** patterns

## Example Improvements

Before:
```lisp
(defun expand-form (form)
  (let ((head (car form))
        (arg1 (cadr form))
        (arg2 (caddr form)))
    ...))
```

After:
```lisp
(defun expand-form (form)
  (let ((head (first form))
        (arg1 (second form))
        (arg2 (third form)))
    ...))
```

## Scope

This is a **cleanup** task, not a rewrite:
- Don't change the expansion algorithm
- Don't change the interface
- Just improve readability and use stdlib

## Testing

1. Test quasiquote: `` `(foo ~x ~@xs) ``
2. Test gensym: `(defmacro let1 ...)`
3. Run: `USE_LIARLIAR=1 cargo test --package liar-spec`

## Can Run In Parallel With

- `f75jg` (symbols)
- `fdrg3` (env)
- `jsoyp` (printer)

## Acceptance Criteria

- [ ] Uses `first`/`rest` instead of `car`/`cdr`
- [ ] Uses `second`/`third` where appropriate
- [ ] All macro tests pass
- [ ] Line count < 200 (modest reduction)
