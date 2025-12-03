## Summary
Add guaranteed tail call instruction to lIR.

## Syntax
```lisp
(tailcall @function args...)
```

## AST Addition
```rust
// In ast.rs
pub enum Instruction {
    // ... existing ...
    TailCall {
        func: String,
        args: Vec<Expr>,
    },
}
```

## Parser Addition
```rust
// In parser.rs, parse_instruction()
"tailcall" => {
    let func = self.parse_func_ref()?;
    let args = self.parse_args()?;
    Ok(Instruction::TailCall { func, args })
}
```

## Codegen
```rust
// In codegen.rs
Instruction::TailCall { func, args } => {
    let fn_val = self.get_function(&func)?;
    let arg_vals: Vec<_> = args.iter()
        .map(|a| self.compile_expr(a))
        .collect::<Result<_>>()?;
    
    let call = self.builder.build_call(fn_val, &arg_vals, "tailcall");
    call.set_tail_call(true);
    self.builder.build_return(Some(&call.try_as_basic_value().left().unwrap()));
}
```

## Verifier Checks
1. Must be in tail position (last instruction before implicit ret)
2. Return type must match function return type
3. No owned values pending drop after tailcall

## Test Cases
```gherkin
Scenario: Simple tail call
  Given the expression (define (loop void) () (block entry (tailcall @loop)))
  Then compilation succeeds

Scenario: Tail recursive factorial
  Given the expression (define (fact-tail i64) ((i64 n) (i64 acc)) (block entry (br (icmp eq n (i64 0)) done recurse)) (block done (ret acc)) (block recurse (tailcall @fact-tail (sub n (i64 1)) (mul n acc))))
  When I call fact-tail with (i64 5) (i64 1)
  Then the result is (i64 120)

Scenario: Mutual tail recursion
  Given the expression (define (even? i1) ((i64 n)) (block entry (br (icmp eq n (i64 0)) yes no)) (block yes (ret (i1 1))) (block no (tailcall @odd? (sub n (i64 1)))))
  And the expression (define (odd? i1) ((i64 n)) (block entry (br (icmp eq n (i64 0)) no yes)) (block no (ret (i1 0))) (block yes (tailcall @even? (sub n (i64 1)))))
  # Test via wrapper returning i64
```

## Acceptance Criteria
- [ ] `tailcall` parses
- [ ] Codegen emits LLVM tail call
- [ ] Tail position verified
- [ ] Return type verified
- [ ] Feature file passes
