# Stdlib Deps Validation

## Summary

Write a test file that validates liar's stdlib works for liarliar's needs. This is a **blocking prerequisite** for all other stdlib rewrite moths.

## Status: COMPLETE

The validation test passes. All core dependencies work but with significant limitations documented below.

## What Works

1. **liar.seq**: `cons`, `first`, `rest`, `second`, `third`, `fourth` all work correctly
2. **liar.hashmap**: `hash-map`, `hm-assoc`, `hm-get` work with integer keys
3. **liar.core**: `inc`, `dec`, and other core functions work

## CRITICAL LIMITATIONS DISCOVERED

### 1. Cons stores head as i64, not ptr
- **Impact**: Liarliar needs to store AST nodes (ptrs) in lists
- **Workaround**: Can cast i64 to/from ptr if needed, OR store type-tagged pointers
- **Fix needed**: Consider adding PtrCons struct OR changing Cons.head to ptr

### 2. HashMap only supports integer keys
- **Impact**: Liarliar uses string keys for symbol tables
- **Workaround**: Hash strings to integers, OR use integer symbol IDs
- **Fix needed**: hash-key function only handles integers, need string hashing

### 3. HashMap protocol dispatch (get/assoc) crashes
- **Impact**: Can't use `(get m k)` or `(assoc m k v)` via Lookup/Associative protocols
- **Workaround**: Use `hm-get` and `hm-assoc` directly (they work)
- **Fix needed**: Debug protocol dispatch for HashMap type

### 4. Threading macro doesn't work
- **Impact**: Can't use `(-> x f g)` sugar in liarliar
- **Workaround**: Manually expand to nested calls `(g (f x))`
- **Note**: This may be a parser/macro-time issue

## Test File

Created `liarliar/test-stdlib-deps.liar` which validates the working functionality.
Run with:
```bash
cargo run --release --bin liarc -- liarliar/test-stdlib-deps.liar > /tmp/test-deps.lir
./target/release/lair /tmp/test-deps.lir -o /tmp/test-deps
/tmp/test-deps
```

Expected output: `All tests passed!` with exit code 0.

## Recommendations for Other Moths

Given these limitations:

1. **f75jg (Symbols HashMap)**: Use integer symbol IDs, not string keys
2. **fdrg3 (Env HashMap)**: Use integer symbol IDs as keys
3. **gr4zj (Reader Seq)**: Cons works for integers; may need ptr workaround for AST
4. **jsoyp (Printer)**: Can proceed as planned
5. **uhgbj (Expand)**: Can proceed as planned
6. **aa01i (Codegen)**: Can proceed as planned

## Acceptance Criteria

- [x] `test-stdlib-deps.liar` compiles with liarc
- [x] `test-stdlib-deps.liar` runs and passes (exit 0)
- [x] Limitations documented for other moths

## Blocks

All other stdll-* moths can now proceed with awareness of limitations.
