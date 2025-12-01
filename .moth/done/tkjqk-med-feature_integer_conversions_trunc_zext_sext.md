# Feature: Integer Conversions (trunc, zext, sext)

Write `cert/features/integer_conversions.feature` covering:

- `trunc` to smaller types
- `zext` zero extension
- `sext` sign extension

## Implementation Notes

Created `cert/features/integer_conversions.feature` with scenarios for:

1. **trunc** - Truncate to smaller integer (keeps low bits)
2. **zext** - Zero extend to larger integer (pads with 0s)
3. **sext** - Sign extend to larger integer (replicates sign bit)

Syntax: `(trunc target-type source-value)` - note target type comes first

Key design decisions:
- trunc keeps only the low bits, discards high bits
- zext treats source as unsigned, pads high bits with 0
- sext treats source as signed, replicates sign bit
- i1 extends: zext i32 (i1 1) = 1, sext i32 (i1 1) = -1

zext vs sext example:
- (zext i16 (i8 128)) = 128 (0x0080, bit pattern preserved)
- (sext i16 (i8 128)) = -128 (0xFF80, sign extended)
