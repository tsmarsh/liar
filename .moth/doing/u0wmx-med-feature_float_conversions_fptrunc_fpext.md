# Feature: Float Conversions (fptrunc, fpext)

Write `cert/features/float_conversions.feature` covering:

- `fptrunc` double to float
- `fpext` float to double

## Implementation Notes

Created `cert/features/float_conversions.feature` with scenarios for:

1. **fptrunc** - Truncate to smaller float type (double -> float)
2. **fpext** - Extend to larger float type (float -> double)

Syntax: `(fptrunc target-type source-value)` - target type first

Key design decisions:
- fptrunc may lose precision (double has more bits)
- fptrunc may overflow to inf if value exceeds float range
- fpext preserves value exactly (float fits in double)
- Special values (inf, nan, -0.0) preserved through conversions
- Rounding follows IEEE 754 rules
