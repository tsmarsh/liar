## Summary
Add local variable references (SSA values).

## Syntax
```lisp
%name       ; reference a named value
```

## Examples
```lisp
(define (double-it i32) ((i32 x))
  (ret (add %x %x)))

; With let bindings for intermediate values
(define (example i32) ()
  (let ((a (i32 5))
        (b (add %a (i32 3))))
    (ret %b)))
```

## Design Options
In true SSA, every assignment creates a new value. Options for binding:

1. **Implicit numbering**: results are %0, %1, %2... (raw LLVM style)
2. **Explicit let binding**: `(let ((name expr)) body)` (Lisp style)
3. **Named results**: `(= name expr)` (assignment style)

Recommend option 2 (let) for consistency with liar.

## Acceptance Criteria
- [ ] Feature file in `cert/features/variables.feature`
- [ ] Parse variable references (%name)
- [ ] Track SSA values
- [ ] Parameters accessible by name
- [ ] Let bindings (if chosen)

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-019**: Variable binding strategy affects how liar emits lIR.
