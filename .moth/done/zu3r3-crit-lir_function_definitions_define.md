## Summary
Add function definition support to lIR.

## Syntax
```lisp
(define (name return-type) ((param-type param-name) ...) body...)
```

## Examples
```lisp
(define (add-one i32) ((i32 x))
  (ret (add x (i32 1))))

(define (get-42 i32) ()
  (ret (i32 42)))

(define (void-fn void) ()
  (ret))
```

## LLVM IR Output
```llvm
define i32 @add-one(i32 %x) {
entry:
  %0 = add i32 %x, 1
  ret i32 %0
}
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature`
- [ ] Parse function definitions
- [ ] Generate LLVM function
- [ ] Handle parameters
- [ ] Handle void return type
- [ ] Entry block created automatically

## Notes
- **Spec-first**: Write the feature file BEFORE implementing. Tests have three states: green (passing), yellow (not implemented), red (broken). See CLAUDE.md.
- **ADR-019**: All frontends target lIR. This is foundational for liar to emit.
- **ADR-020**: lair (the assembler) is written in Rust and emits LLVM IR via inkwell.
