# Bit Operation Primitives for HAMT

**Priority:** high
**Category:** lIR + liar/codegen
**Dependencies:** none

## Summary

Add bit manipulation primitives required for implementing HAMT (Hash Array Mapped Trie) persistent collections in pure liar. The goal is to write PersistentVector and PersistentHashMap in liar itself, validating ownership/borrow/arc through the full pipeline.

## What's Needed

### 1. lIR: popcount (ctpop)

HAMT uses popcount to compute array indices from sparse bitmaps. LLVM provides `@llvm.ctpop.i64`.

**lir-core/src/ast.rs:**
```rust
/// Count set bits (population count)
Ctpop(Box<Expr>),
```

**lir-core/src/display.rs:**
```rust
Expr::Ctpop(val) => write!(f, "(ctpop {})", val),
```

**lir-core/src/parser.rs:**
```rust
"ctpop" => {
    let val = parse_expr(iter)?;
    Ok(Expr::Ctpop(Box::new(val)))
}
```

**lir-codegen (AOT):**
```rust
Expr::Ctpop(val) => {
    let v = self.compile_expr(val)?;
    let intrinsic = self.module.get_intrinsic("llvm.ctpop", &[self.context.i64_type().into()])?;
    self.builder.build_call(intrinsic, &[v.into()], "ctpop")
}
```

**lir-codegen (JIT):** Similar pattern using cranelift's `popcnt` instruction.

### 2. liar builtins: Integer bitwise ops

lIR already has And/Or/Xor/Shl/Shr but liar's `and`/`or` are boolean. Need distinct integer bitwise ops.

**liar/src/codegen/builtins.rs:**
```rust
// Integer bitwise (distinct from boolean and/or)
"bit-and" => {
    check_binary(expr, "bit-and", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::And(Box::new(a), Box::new(b)))
}
"bit-or" => {
    check_binary(expr, "bit-or", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::Or(Box::new(a), Box::new(b)))
}
"bit-xor" => {
    check_binary(expr, "bit-xor", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::Xor(Box::new(a), Box::new(b)))
}
"bit-not" => {
    check_unary(expr, "bit-not", args)?;
    let a = generate_expr(ctx, &args[0])?;
    // XOR with -1 (all bits set)
    Some(lir::Expr::Xor(
        Box::new(lir::Expr::IntLit { ty: lir::ScalarType::I64, value: -1 }),
        Box::new(a),
    ))
}
"bit-shift-left" | "shl" => {
    check_binary(expr, "bit-shift-left", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::Shl(Box::new(a), Box::new(b)))
}
"bit-shift-right" | "shr" => {
    check_binary(expr, "bit-shift-right", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::LShr(Box::new(a), Box::new(b)))  // logical shift
}
"arithmetic-shift-right" | "ashr" => {
    check_binary(expr, "arithmetic-shift-right", args)?;
    let a = generate_expr(ctx, &args[0])?;
    let b = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::AShr(Box::new(a), Box::new(b)))
}
"popcount" => {
    check_unary(expr, "popcount", args)?;
    let a = generate_expr(ctx, &args[0])?;
    Some(lir::Expr::Ctpop(Box::new(a)))
}
```

## Tests

### lIR level (features/bitwise.feature)
```gherkin
Scenario: popcount counts set bits
  Given lIR code:
    """
    (define main () -> i64
      (entry)
      (ret (ctpop (i64 0b1010101))))
    """
  When I compile and run it
  Then the exit code should be 4

Scenario: popcount of zero
  Given lIR code:
    """
    (define main () -> i64
      (entry)
      (ret (ctpop (i64 0))))
    """
  When I compile and run it
  Then the exit code should be 0

Scenario: popcount of all ones
  Given lIR code:
    """
    (define main () -> i64
      (entry)
      (ret (ctpop (i64 -1))))
    """
  When I compile and run it  
  Then the exit code should be 64
```

### liar level (lib/stdlib-tests.liar or similar)
```lisp
;; Bitwise operations
(assert (= (bit-and 0b1100 0b1010) 0b1000))
(assert (= (bit-or 0b1100 0b1010) 0b1110))
(assert (= (bit-xor 0b1100 0b1010) 0b0110))
(assert (= (bit-shift-left 1 4) 16))
(assert (= (bit-shift-right 16 2) 4))
(assert (= (popcount 0b1010101) 4))
(assert (= (popcount 0) 0))

;; HAMT index calculation pattern
(defun hamt-index (hash shift)
  (bit-and (bit-shift-right hash shift) 0x1f))  ;; 5 bits = 32-way

(defun hamt-mask (hash shift)
  (bit-shift-left 1 (hamt-index hash shift)))

(assert (= (hamt-index 0b11010_00000 5) 0b11010))  ;; 26
(assert (= (hamt-mask 0b00011_00000 5) (bit-shift-left 1 3)))  ;; bit 3 set
```

## Acceptance Criteria

- [ ] `ctpop` added to lir-core AST
- [ ] `ctpop` parsing works
- [ ] `ctpop` AOT codegen emits `@llvm.ctpop.i64`
- [ ] `ctpop` JIT codegen emits cranelift `popcnt`
- [ ] liar builtins: `bit-and`, `bit-or`, `bit-xor`, `bit-not`
- [ ] liar builtins: `bit-shift-left`/`shl`, `bit-shift-right`/`shr`, `ashr`
- [ ] liar builtin: `popcount`
- [ ] All tests pass
- [ ] HAMT index/mask helpers work in liar

## Notes

This is prerequisite infrastructure. Once complete, PersistentVector and PersistentHashMap can be implemented in pure liar, which validates the ownership/borrow/arc system on real recursive data structures.
