## Summary

Empty parameter lists in lIR were printed as the symbol `nil`, causing lair parse errors for zero-arg functions.

## Root Cause

`liarliar/printer.liar` and `liarliar/codegen.liar` both defined `str-nil`, which produced a symbol-name collision in the compiled output. The printer picked up the codegen version (`"nil"`) instead of its own (`"()"`).

## Fix

Renamed the printer helper to `str-empty-list` and updated its usage so empty lists print as `()`.

## Verification

- Rebuilt liarliar and compiled:
  - `/tmp/zero.liar` now emits `(define (foo i64) () ...)` instead of `nil`.
- Re-ran stdlib smoke loop; nil param parse errors are gone (new gaps logged in `doc/stdll-gaps.md`).
