# Fix Incremental JIT on macOS ARM

## Problem

The REPL's incremental JIT returns incorrect values on macOS ARM (Apple Silicon).

Simple expressions like `(+ 1 2)` work correctly, but when defining functions
and then calling them, the JIT returns what look like memory addresses instead
of the computed values.

## Evidence

Tests `test_definition_and_call` and `test_multiple_definitions` in
`liar-repl/src/session.rs` fail on macOS:

- Expected `6`, got `4347294545` (0x103300B71)
- Expected `10`, got `8694589088` (0x206580AA0)

## Notes

- Certification tests (single-shot compilation) pass on macOS
- This suggests the issue is with incremental module linking, not codegen
- The issue may be related to LLVM JIT calling conventions on macOS ARM
- Tests are currently `#[ignore]` on macOS pending fix

## Files

- `liar-repl/src/session.rs` - REPL session with incremental JIT
