# Liarliar Env HashMap

## Summary

Replace the manual environment implementation in `liarliar/env.liar` with `liar.hashmap`.

## Current State

`env.liar` (~150 lines) implements:
- `Env` struct with bindings list and parent pointer
- `env-lookup` with manual list traversal
- `env-define` with cons-based binding addition
- Various helper functions

## Target State

~40 lines using HashMap:

```lisp
(ns liarliar.env
  (:require [liar.hashmap :refer :all]))

(defstruct Env (env-bindings: ptr env-parent: ptr))

(defun make-env () -> ptr
  (share (Env (hashmap) nil)))

(defun make-env-with-parent (parent: ptr) -> ptr
  (share (Env (hashmap) parent)))

(defun env-lookup (env: ptr name: ptr) -> ptr
  (if (nil? env)
      nil
      (let ((local (get (. env env-bindings) name)))
        (if (nil? local)
            (env-lookup (. env env-parent) name)
            local))))

(defun env-define (env: ptr name: ptr value: ptr) -> ptr
  (share (Env (assoc (. env env-bindings) name value)
              (. env env-parent))))

(defun env-extend (env: ptr names: ptr values: ptr) -> ptr
  ;; Extend env with multiple bindings at once
  (if (nil? names)
      env
      (env-extend (env-define env (first names) (first values))
                  (rest names)
                  (rest values))))
```

## Key Changes

1. Replace binding list with HashMap
2. Simplify lookup (no manual list traversal)
3. Leverage persistent data structure for env-define
4. Keep parent chain for lexical scoping

## Interface Contract

These functions must keep compatible signatures:
- `(make-env) -> ptr`
- `(env-lookup env name) -> ptr`
- `(env-define env name value) -> ptr`

## Testing

1. Rebuild liarliar with new env.liar
2. Run: `/tmp/liarliar liarliar/test-codegen.liar`
3. Test closure scenarios that rely on env lookup

## Depends On

- `n02qm` (stdlib deps) - HashMap must work

## Can Run In Parallel With

- `f75jg` (symbols)
- `jsoyp` (printer)
- `uhgbj` (expand)

## Acceptance Criteria

- [ ] env.liar uses `liar.hashmap`
- [ ] No manual list traversal for lookups
- [ ] All liarliar tests pass
- [ ] Line count < 50
