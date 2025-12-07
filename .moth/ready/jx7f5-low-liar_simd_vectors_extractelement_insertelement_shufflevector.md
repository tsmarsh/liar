## Summary

Add SIMD vector operations to liar.

## liar Syntax

```lisp
;; Create vector
(vec4 i32 1 2 3 4)

;; Extract element
(vec-get v 0)

;; Insert element
(vec-set v 2 99)

;; Shuffle
(vec-shuffle v1 v2 [0 4 1 5])  ; interleave
```

## lIR Output

```lisp
(extractelement <4 x i32> %v (i32 0))
(insertelement <4 x i32> %v (i32 99) (i32 2))
(shufflevector <4 x i32> %v1 <4 x i32> %v2 <4 x i32> [0 4 1 5])
```

## Use Cases

- Numeric computation
- Graphics/audio processing
- Parallel data operations

## Acceptance Criteria

- [ ] Parse vector literal syntax
- [ ] Parse vector operations
- [ ] Generate correct lIR vector ops
- [ ] Feature file with test scenarios

## Notes

Low priority - specialized use case. lIR already supports these fully.
