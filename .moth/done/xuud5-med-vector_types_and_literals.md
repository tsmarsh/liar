# Vector Types and Literals

## Goal
Implement vector types and literals in JIT codegen.

## Covered by
- cert/features/vector_types.feature

## Tasks
1. Codegen for VectorLit - create LLVM vector constants
2. JIT return type handling for vectors
3. Value display for vectors (e.g., `(<4 x i32> 1 2 3 4)`)
4. Vector arithmetic (add, fadd, mul, etc. already work element-wise in LLVM)
5. Vector comparison (icmp, fcmp on vectors)

## Notes
- LLVM vectors use `<N x type>` syntax
- Vector operations are element-wise by default
- inkwell: `int_type.vec_type(count)` and `float_type.vec_type(count)`
- Results are `<N x i1>` for vector comparisons
