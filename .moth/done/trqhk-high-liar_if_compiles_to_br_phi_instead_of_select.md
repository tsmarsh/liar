## Summary

Change liar's `if` expression to compile to `br`/`phi` (proper control flow) instead of `select` (eager evaluation of both branches).

## Problem

Currently `if` compiles to LLVM's `select` instruction, which evaluates both branches before choosing. This breaks recursive functions that depend on short-circuit evaluation:

```lisp
(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
```

With `select`, both branches are evaluated, causing infinite recursion even when `n <= 1`.

## Solution

Generate proper basic blocks with conditional branches:

```llvm
entry:
  %cond = icmp sle i64 %n, 1
  br i1 %cond, label %then, label %else

then:
  br label %merge

else:
  %rec = call i64 @factorial(i64 %n_minus_1)
  %result = mul i64 %n, %rec
  br label %merge

merge:
  %phi = phi i64 [ 1, %then ], [ %result, %else ]
  ret i64 %phi
```

## Implementation

1. Add block management to `CodegenContext` (current block, block list)
2. Refactor `generate_defun` to build multiple blocks
3. Change `generate_if` in `control.rs` to:
   - Emit conditional `br` to then/else labels
   - Create then-block, generate then-expr, emit `br` to merge
   - Create else-block, generate else-expr, emit `br` to merge
   - Create merge-block with `phi` node
4. Update tests

## Acceptance Criteria

- [ ] `factorial`, `fib`, `gcd`, `pow`, `sum-to` stdlib tests pass
- [ ] Feature file scenarios for recursive functions work
- [ ] Non-recursive `if` still works (regression tests)

## Blocked By

None - lIR already supports `br` and `phi`.

## Enables

- Recursive stdlib functions (factorial, fib, gcd, lcm, pow, sum-to)
- `match` expressions
- General control flow patterns
