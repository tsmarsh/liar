## Summary
Update the lIR language documentation to cover all implemented features.

## Sections needed

### Types
- Scalars: i1, i8, i16, i32, i64, float, double, void
- Pointer: ptr
- Vectors: <N x type>
- Structs: %struct.name

### Literals
- Integers: (i32 42)
- Floats: (double 3.14), inf, -inf, nan
- Pointer: (ptr null)
- String: (string "hello\n")
- Vector: (<4 x i32> 1 2 3 4)
- Struct: { (i32 1) (i32 2) }

### Instructions
Document all with examples:
- Arithmetic: add, sub, mul, sdiv, udiv, srem, urem
- Float: fadd, fsub, fmul, fdiv, frem
- Bitwise: and, or, xor, shl, lshr, ashr
- Comparison: icmp, fcmp
- Conversion: trunc, zext, sext, fptrunc, fpext, etc.
- Memory: alloca, load, store, getelementptr
- Control: br, phi, ret
- Aggregate: extractvalue, insertvalue
- Other: select, call

### Top-level forms
- (define ...)
- (declare ...)
- (global ...)
- (defstruct ...)

### Let bindings
Document the syntax once implemented.

## Acceptance Criteria
- [ ] All types documented
- [ ] All instructions documented with examples
- [ ] Top-level forms documented
- [ ] Let bindings documented
