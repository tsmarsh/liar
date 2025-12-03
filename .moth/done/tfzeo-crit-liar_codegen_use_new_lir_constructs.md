# Liar Codegen: Use New lIR Constructs

## Summary
Update liar's codegen to use the new ownership and memory management constructs added to lIR:
- Ownership types (`own T`, `ref T`, `refmut T`)
- Borrow operations (`alloc own`, `borrow ref/refmut`, `drop`, `move`)
- Reference counting (`rc T`, `rc-alloc`, `rc-clone`, `rc-drop`, `rc-ptr`)
- Bounds-checked arrays (`array-alloc`, `array-get`, `array-set`, `array-len`)

## Current State
The liar codegen (`liar/src/codegen.rs`) currently:
- Uses raw pointers for references
- Has placeholders for `Ref` and `RefMut` that just pass through
- Uses `(store ...)` for mutations without proper ownership tracking
- Has no array support beyond raw pointers

## Required Changes

### 1. Type Generation (`liar_type_to_lir`)
Map liar types to lIR ownership types:
```rust
Type::Ref(inner) => format!("ref {}", liar_type_to_lir(inner)),
Type::RefMut(inner) => format!("refmut {}", liar_type_to_lir(inner)),
// Add Owned type mapping if liar has it
```

### 2. Reference Expressions
Update `Expr::Ref` and `Expr::RefMut`:
```rust
Expr::Ref(inner) => {
    let inner = generate_expr(inner)?;
    Ok(format!("(borrow ref {})", inner))
}
Expr::RefMut(inner) => {
    let inner = generate_expr(inner)?;
    Ok(format!("(borrow refmut {})", inner))
}
```

### 3. Let Bindings with Owned Values
For bindings that allocate, use `alloc own`:
```rust
// When binding creates owned value
format!("(let (({} (alloc own {}))) ...)", name, elem_type)
```

### 4. Drop at Scope End
Insert drops for owned values at scope boundaries:
```rust
// At end of let scope for owned bindings
format!("(drop {})", binding_name)
```

### 5. Array Support
Add array literal support:
```rust
Expr::Array(elements) => {
    let len = elements.len();
    let elem_type = infer_element_type(elements)?;
    // Generate array-alloc + array-set for each element
}
```

### 6. RC Types (if liar supports them)
For shared ownership types, use RC constructs:
```rust
// For Rc<T> in liar
Expr::RcNew(inner) => {
    let inner = generate_expr(inner)?;
    Ok(format!("(let ((rc (rc-alloc {}))) (store {} (rc-ptr rc)) rc)", elem_type, inner))
}
```

## Integration Points
- `ownership.rs` - Already tracks ownership/borrowing at liar level
- `closures.rs` - Captured variables may need RC
- `infer.rs` - Type information needed for codegen

## Test Cases
Add liar programs that compile to:
1. Owned allocations with drops
2. Shared and mutable borrows
3. Reference-counted values
4. Bounds-checked arrays

## Acceptance Criteria
- [ ] `&x` compiles to `(borrow ref x)`
- [ ] `&mut x` compiles to `(borrow refmut x)`
- [ ] Owned values get `(drop x)` at scope end
- [ ] Arrays compile to bounds-checked operations
- [ ] All liar ownership tests pass
- [ ] Generated lIR passes borrow checker
