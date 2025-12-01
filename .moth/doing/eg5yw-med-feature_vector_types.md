# Feature: Vector Types

Write `cert/features/vector_types.feature` covering:

- `<N x type>` syntax
- Integer and float element types
- Various vector sizes

## Implementation Notes

Created `cert/features/vector_types.feature` with scenarios for:

1. **Integer vector literals** - <4 x i32>, <2 x i64>, <8 x i8>, etc.
2. **Float vector literals** - <4 x float>, <2 x double>
3. **Negative values** - Vectors can contain negative numbers
4. **i1 vectors** - Boolean vectors for masks
5. **Common SIMD sizes** - 128-bit (4xi32, 4xfloat, 2xdouble, 8xi16)
6. **Vector arithmetic** - add, fadd, mul work element-wise
7. **Vector comparison** - icmp, fcmp return i1 vectors
8. **Vector splat** - All elements same value

Syntax: `(<N x type> elem1 elem2 ... elemN)`

Key design decisions:
- Fixed-size vectors (not scalable)
- Element count must match type
- Operations work element-wise
- Comparisons produce <N x i1> results
- Standard SIMD sizes supported
