# Liarliar Printer Simplify

## Status: COMPLETE

Removed dead code from printer.liar and strings.liar, achieving a ~30% reduction.

## Results

| File | Before | After | Reduction |
|------|--------|-------|-----------|
| strings.liar | 226 lines | 153 lines | -73 lines |
| printer.liar | 259 lines | 190 lines | -69 lines |
| **Total** | **485 lines** | **343 lines** | **-142 lines (29%)** |

## Dead Code Removed

### strings.liar
- `string-builder()` - never called (uses `string-builder-cap` instead)
- `nth` / Indexable protocol - never called
- `pop` in Collection protocol - never called
- `to-string-copy` - never called
- All convenience functions: `append-char`, `append-newline`, `append-space`, `append-paren-open`, `append-paren-close`, `appendln`, `append-intln`
- `free` extern (never used)

### printer.liar
- Entire indentation system (never used):
  - `PrinterCtx.pr-indent` field
  - `pr-indent` accessor
  - `pr-indent-inc`, `pr-indent-dec`
  - `emit-indent-loop`, `emit-indent`
- `sexp-to-string` (only `forms-to-string` is called externally)
- Simplified by passing StringBuilder directly instead of through PrinterCtx wrapper

## Why Not 80-100 Lines?

The 80-100 line target was optimistic. The remaining code is all functional:
- Integer-to-string formatting (with reversal)
- Float formatting (with leading zeros for fractions)
- String escaping (newlines, quotes)
- Print dispatcher for 8+ value types (symbol, int, bool, float, string, scons, pcons, icons)

No stdlib string building operations exist that would allow further simplification.

## Testing

- liarliar compiles successfully
- 72 spec scenarios pass (same as before)
- stdlib deps validation test passes

## Acceptance Criteria

- [x] strings.liar simplified (226 → 153 lines)
- [x] printer.liar simplified (259 → 190 lines)
- [x] Dead code removed
- [x] Output still valid lIR
- [x] All liarliar tests pass
