# Liarliar Symbols HashMap

## Summary

Replace the hand-rolled symbol table in `liarliar/symbols.liar` with `liar.hashmap`.

## Current State

`symbols.liar` (~110 lines) implements:
- `Symbol` struct with id and name
- `SymbolTable` struct with manual array storage
- Linear scan for lookup (`find-symbol-loop`)
- Manual id assignment (`add-symbol`)
- `intern` function

## Target State

~30 lines using HashMap:

```lisp
(ns liarliar.symbols
  (:require [liar.hashmap :refer :all]))

(defstruct Symbol (sym-id: i64 sym-name: ptr))

(defstruct SymbolTable (sym-map: ptr sym-count: i64))

(defun make-symbol-table () -> ptr
  (share (SymbolTable (hashmap) 0)))

(defun intern (table: ptr name: ptr) -> ptr
  (let ((m (. table sym-map))
        (existing (get m name)))
    (if (nil? existing)
        ;; New symbol
        (let ((id (. table sym-count))
              (sym (share (Symbol id name)))
              (new-map (assoc m name sym))
              (new-table (share (SymbolTable new-map (+ id 1)))))
          (pcons new-table sym))
        ;; Existing symbol
        (pcons table existing))))

(defun symbol? (x: ptr)
  (instance? x Symbol))

(defun symbol-name (s: ptr) -> ptr
  (. s sym-name))

(defun symbol-id (s: ptr) -> i64
  (. s sym-id))
```

## Key Changes

1. Replace `SymbolTable.sym-tab-data` (ptr array) with `sym-map` (HashMap)
2. Replace `find-symbol-loop` (O(n) linear scan) with `get` (O(log32 n))
3. Remove `sym-table-cap` - HashMap grows automatically
4. Keep `intern` signature compatible

## Interface Contract

These functions must keep the same signatures:
- `(make-symbol-table) -> ptr`
- `(intern table name) -> (pcons new-table symbol)`
- `(symbol? x) -> bool`
- `(symbol-name s) -> ptr`
- `(symbol-id s) -> i64`

## Testing

1. Rebuild liarliar with new symbols.liar
2. Run: `/tmp/liarliar liarliar/test-codegen.liar`
3. Run full spec: `USE_LIARLIAR=1 cargo test --package liar-spec`

## Depends On

- `n02qm` (stdlib deps) - HashMap must work with string keys

## Acceptance Criteria

- [ ] symbols.liar uses `liar.hashmap`
- [ ] No manual array management code
- [ ] All liarliar tests pass
- [ ] Line count < 40
