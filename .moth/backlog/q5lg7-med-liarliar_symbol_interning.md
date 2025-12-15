# liarliar: Symbol Interning

Create `lib/liarliar/symbols.liar` - intern symbols for fast equality comparison.

**Priority:** HIGH (required by reader and all name-based lookups)

## Related ADRs

- [ADR 019: lIR as Universal Backend](../../doc/adr/019-lir-universal-backend.md) — Symbols map to lIR identifiers
- [ADR 006: No Redefinition in Same Scope](../../doc/adr/006-no-redefinition-in-scope.md) — Symbol table enforces uniqueness

## Overview

Instead of string comparison for every symbol lookup, intern symbols to unique IDs. This is critical for performance — the compiler does millions of symbol comparisons.

## Data Structure

```clojure
(defstruct SymbolTable (strings: ptr ids: ptr next-id: i64))

(defun make-symbol-table ()
  (share (SymbolTable (str-map) (hashmap) 0)))
```

## Functions to Implement

```clojure
;; Intern a symbol string, return its ID
(defun intern (table name-ptr)
  (let ((existing (sm-get (. table strings) name-ptr)))
    (if existing
        existing
        (let ((id (. table next-id)))
          ;; Return new table + id (immutable style)
          {:table (update-table table name-ptr id) :id id}))))

;; Look up symbol name by ID
(defun symbol-name (table sym-id)
  (hm-get (. table ids) sym-id))

;; Check if symbol is interned
(defun interned? (table name-ptr)
  (sm-contains? (. table strings) name-ptr))
```

## Built-in Symbols

Pre-intern common symbols:
- `defun`, `def`, `defmacro`, `defstruct`
- `let`, `fn`, `if`, `do`, `cond`
- `quote`, `quasiquote`, `unquote`
- `+`, `-`, `*`, `/`, `=`, `<`, `>`
- `nil`, `true`, `false`

## Dependencies

- `liar.hashmap` - `sm-assoc`, `sm-get`, `hm-assoc`, `hm-get`
- `lib/liarliar/value.liar` - Symbol tagging

## Test Cases

- Intern same string twice returns same ID
- Different strings get different IDs
- Can retrieve name from ID
- Builtins are pre-interned with known IDs

## Design Notes

Consider using a perfect hash for builtins since they're known at compile time. This gives O(1) lookup for the most common symbols.

Keywords (`:foo`) could share the string table with symbols or have a separate namespace. Separate is cleaner — `foo` and `:foo` can coexist without confusion.

## Ordering

Depends on: `value.liar` (for tagged pointers)
Required by: `reader.liar`, `resolve.liar`, `expand.liar`
