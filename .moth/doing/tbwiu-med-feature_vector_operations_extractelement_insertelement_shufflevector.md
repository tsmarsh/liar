# Feature: Vector Operations (extractelement, insertelement, shufflevector)

Write `cert/features/vector_ops.feature` covering:

- `extractelement`
- `insertelement`
- `shufflevector`

## Implementation Notes

Created `cert/features/vector_ops.feature` with scenarios for:

1. **extractelement** - Get single element from vector
   - Syntax: `(extractelement vector index)`
   - Returns scalar type matching element type
   - Index is i32

2. **insertelement** - Replace element in vector
   - Syntax: `(insertelement vector value index)`
   - Returns new vector with one element replaced
   - Original vector unchanged (functional style)

3. **shufflevector** - Rearrange/combine elements from two vectors
   - Syntax: `(shufflevector vec1 vec2 mask)`
   - Mask indices: 0 to N-1 select from vec1, N to 2N-1 from vec2
   - Common patterns: reverse, broadcast, interleave

Key design decisions:
- Index is always i32 type
- Shufflevector mask is a vector of i32
- Operations work with any vector element type
- Chained operations supported
