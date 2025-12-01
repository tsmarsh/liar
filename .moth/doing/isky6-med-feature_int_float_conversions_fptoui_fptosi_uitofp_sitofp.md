# Feature: Int/Float Conversions (fptoui, fptosi, uitofp, sitofp)

Write `cert/features/int_float_conversions.feature` covering:

- `fptoui`, `fptosi` float to int
- `uitofp`, `sitofp` int to float

## Implementation Notes

Created `cert/features/int_float_conversions.feature` with scenarios for:

1. **fptoui** - Float to unsigned integer (truncates toward zero)
2. **fptosi** - Float to signed integer (truncates toward zero)
3. **uitofp** - Unsigned integer to float
4. **sitofp** - Signed integer to float

Syntax: `(fptoui target-type source-value)` - target type first

Key design decisions:
- Float to int truncates toward zero (not floor)
- fptoui treats result as unsigned: (fptoui i8 255.0) = -1 (bit pattern)
- uitofp treats source as unsigned: (uitofp double (i8 -1)) = 255.0
- sitofp treats source as signed: (sitofp double (i8 -1)) = -1.0
- Large integers may lose precision when converted to float
