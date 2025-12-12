## Summary

Add float/int conversion operations to liar.

## liar Syntax

```lisp
;; Float precision
(float->double x)    ; fpext
(double->float x)    ; fptrunc

;; Float to int
(float->int x)       ; fptosi (signed)
(float->uint x)      ; fptoui (unsigned)

;; Int to float
(int->float x)       ; sitofp (signed)
(uint->float x)      ; uitofp (unsigned)
```

## lIR Output

```lisp
(fpext double (float 1.5))
(fptrunc float (double 1.5))
(fptosi i64 (double 3.7))    ; => 3
(sitofp double (i64 42))     ; => 42.0
```

## Use Cases

- Numeric algorithms mixing int/float
- FFI with C libraries
- Precision control

## Acceptance Criteria

- [ ] Parse all conversion operations
- [ ] Generate correct lIR ops
- [ ] Feature file with test scenarios
- [ ] Proper type checking

## Notes

lIR already supports these. Frontend work only.
