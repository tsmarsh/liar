## Summary
Implement the borrow checker per ADRs 001-007. Track ownership state,
detect use-after-move, verify borrow rules.

## Implementation

Implemented in `liar/src/ownership.rs`:

### Ownership states
```rust
pub enum OwnershipState {
    Owned,              // Value is owned here
    Moved(Span),        // Value has been moved away (with location)
    Borrowed(Vec<BorrowId>), // Immutably borrowed (multiple OK)
    BorrowedMut(BorrowId), // Mutably borrowed (exclusive)
}
```

### Borrow tracking
```rust
pub struct BorrowChecker {
    scopes: Vec<Scope>,
    ownership: HashMap<BindingId, OwnershipState>,
    borrows: HashMap<BorrowId, BorrowInfo>,
    bindings: HashMap<String, Vec<BindingId>>,
    errors: Errors,
}

pub struct BorrowInfo {
    pub kind: BorrowKind,
    pub borrowed_from: BindingId,
    pub span: Span,
    pub scope: ScopeId,
}
```

## Rules enforced

### ADR-001: Immutability by default
- Values are immutable unless explicitly mutated via `&mut`

### ADR-002: Pass by reference
- Function args are treated as owned (can be used multiple times)
- Value moves happen on let bindings

### ADR-003: Mutable reference sigil
- `(ref x)` = shared borrow
- `(ref-mut x)` = exclusive mutable borrow

### ADR-004: Lexical ownership
- Every value has one owner
- Owner controls lifetime
- When scope ends, borrows are released

### ADR-007: Aliasing rules
- Multiple shared borrows OK
- Mutable borrow is exclusive
- No mutable + shared at same time

## Special handling

### If branches
- Fork ownership state before branches
- Check each branch independently
- Merge states after (conservative: moved in either = moved)

### Let bindings
- Value expressions are checked first
- Variable binding moves the value

## Errors detected
- Use after move (with original move location)
- Move of borrowed value
- Mutable borrow while borrowed
- Double mutable borrow
- Shared borrow while mutably borrowed

## Tests (all passing)
- `test_simple_use` - basic variable usage
- `test_use_after_move` - detects use after move
- `test_double_move` - detects second move attempt
- `test_borrow_shared` - allows shared borrow
- `test_multiple_shared_borrows` - multiple &x OK
- `test_mutable_borrow_exclusive` - no double &mut
- `test_shared_and_mutable_conflict` - &x then &mut x fails
- `test_mutable_then_shared_conflict` - &mut x then &x fails
- `test_move_while_borrowed` - can't move borrowed value
- `test_if_branches_independent` - moves in branches don't conflict
- `test_function_params_owned` - params start as owned
- `test_lambda_captures` - lambdas can capture

## Acceptance criteria
- [x] Use-after-move detected
- [x] Borrow rules enforced
- [x] Mutable exclusivity enforced
- [x] Borrow escape detected (move while borrowed)
- [x] Good error messages with spans
- [ ] Drop insertion at scope end (codegen concern, not borrow checking)
