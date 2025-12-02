## Summary
Add a comprehensive integration test that proves lIR is ready for liar to target.
When all scenarios pass, lIR has sufficient capabilities.

## The file
Create `cert/features/integration_milestone.feature` with phases:

1. **Basic Functions** - define, ret, call, params
2. **Control Flow** - blocks, br, conditional br, phi
3. **Recursion/Loops** - recursive calls, phi loops
4. **Memory** - alloca, load, store
5. **External Calls** - declare, FFI
6. **Structs** - defstruct, GEP for field access
7. **Function Pointers** - indirect calls (nice to have)
8. **Closure Simulation** - The ultimate test

## Key tests

### Recursive factorial
```lisp
(define (factorial i64) ((i64 n))
  (block entry
    (br (icmp eq n (i64 0)) base_case recursive_case))
  (block base_case
    (ret (i64 1)))
  (block recursive_case
    (let ((n_minus_1 (sub n (i64 1)))
          (sub_result (call @factorial n_minus_1)))
      (ret (mul n sub_result)))))
```

### Closure simulation (milestone)
```lisp
(defstruct adder_env (i64))

(define (adder_fn i64) ((ptr env) (i64 y))
  (let ((x_ptr (getelementptr %struct.adder_env env (i32 0) (i32 0)))
        (x (load i64 x_ptr)))
    (ret (add x y))))

(define (make_adder ptr) ((i64 x))
  (declare malloc ptr (i64))
  (let ((env (call @malloc (i64 8))))
    (store x (getelementptr %struct.adder_env env (i32 0) (i32 0)))
    (ret env)))
```

## Acceptance Criteria
- [ ] Feature file created
- [ ] All phases documented
- [ ] Final closure simulation test included
- [ ] When all pass, lIR is "A+" ready
