# Vector Operations (extractelement, insertelement, shufflevector)

## Goal
Implement vector manipulation instructions in JIT codegen.

## Covered by
- cert/features/vector_ops.feature

## Tasks
1. extractelement - get element at index from vector
   - `(extractelement <vec> <idx>)` ’ scalar
   - inkwell: `build_extract_element`

2. insertelement - replace element at index in vector
   - `(insertelement <vec> <val> <idx>)` ’ vector
   - inkwell: `build_insert_element`

3. shufflevector - rearrange elements from two vectors
   - `(shufflevector <vec1> <vec2> <mask>)` ’ vector
   - inkwell: `build_shuffle_vector`
   - Mask is a vector of i32 indices

## Notes
- Requires vector types to be working first (xuud5)
- Index is typically i32
- shufflevector mask can reference either input vector (0-3 = vec1, 4-7 = vec2)
