# Feature: Type Mismatch Errors

Write `cert/features/type_errors.feature` covering:

- Mismatched operand types
- Wrong type for operation (e.g., add on floats)
- Invalid conversions

## Implementation Notes

Created `cert/features/type_errors.feature` with scenarios for:

1. **Mismatched integer operand types** - e.g., (add (i8 1) (i32 2))
2. **Mismatched float operand types** - e.g., (fadd (float 1.0) (double 2.0))
3. **Integer ops on floats** - add/sub/mul/div on double/float
4. **Float ops on integers** - fadd/fsub/fmul/fdiv on i32/i64
5. **Bitwise ops on floats** - and/or/xor/shl on float/double
6. **icmp on floats** - requires integer operands
7. **fcmp on integers** - requires float operands
8. **Mismatched comparison operands** - different sized ints/floats
9. **Select type errors** - mismatched values, non-i1 condition
10. **Invalid conversion directions** - trunc to larger, extend to smaller
11. **Wrong type for conversions** - fptosi on int, sitofp on float

Key design decisions:
- All errors caught at compile time (static typing)
- No implicit type promotion or coercion
- Error messages should be descriptive
- This enforces LLVM's strict type system
