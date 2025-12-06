## Summary

`liar/src/closures.rs` is ~1400 lines. Split it into focused modules.

## Current State

Contains:
- Capture analysis (which variables a closure captures)
- Closure color determination (thread safety)
- Thread safety checking for plet/async
- Tests for all of the above

## Target State

```
liar/src/closures/
  mod.rs          - public API, re-exports (~30 lines)
  types.rs        - CaptureMode, ClosureColor, Capture, CaptureInfo (~80 lines)
  analysis.rs     - ClosureAnalyzer, capture detection (~300 lines)
  safety.rs       - ThreadSafetyChecker (~200 lines)
  tests.rs        - all tests (~400 lines)
```

## Steps

1. Create `liar/src/closures/` directory
2. Extract types to `types.rs`
3. Extract analysis to `analysis.rs`
4. Extract safety checking to `safety.rs`
5. Move tests to `tests.rs`
6. Create `mod.rs` with re-exports
7. Update `lib.rs` (should work unchanged due to mod.rs)
8. Run `cargo test -p liar`

## Acceptance Criteria

- [ ] No file in closures/ exceeds 400 lines
- [ ] All existing tests pass
- [ ] Public API unchanged (same imports work)
- [ ] No thread-local state
