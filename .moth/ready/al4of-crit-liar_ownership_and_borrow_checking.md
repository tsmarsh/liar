## Summary
Implement the borrow checker per ADRs 001-007. Track ownership state,
detect use-after-move, verify borrow rules.

## Ownership states
```rust
pub enum OwnershipState {
    Owned,              // Value is owned here
    Moved,              // Value has been moved away
    Borrowed(BorrowId), // Immutably borrowed
    BorrowedMut(BorrowId), // Mutably borrowed
    Shared,             // Reference counted (no move tracking)
}
```

## Borrow tracking
```rust
pub struct BorrowChecker {
    bindings: HashMap<BindingId, OwnershipState>,
    active_borrows: HashMap<BorrowId, BorrowInfo>,
    errors: Vec<BorrowError>,
}

pub struct BorrowInfo {
    pub kind: BorrowKind,
    pub borrowed_from: BindingId,
    pub span: Span,
    pub scope_end: ScopeId,
}
```

## Rules to enforce

### ADR-001: Immutability by default
- Values are immutable unless explicitly mutated via `&mut`

### ADR-002: Pass by reference  
- Function args are references under the hood (optimization)
- But semantically, value types move

### ADR-003: Mutable reference sigil
- `&x` = shared borrow
- `&mut x` = exclusive mutable borrow

### ADR-004: Lexical ownership
- Every value has one owner
- Owner controls lifetime
- When owner goes out of scope, value is dropped

### ADR-005: Closure captures
- Closures capture by move (default)
- Or by borrow if `&x` used in closure
- Closure cannot escape if it borrows

### ADR-006: No redefinition
- Cannot shadow in same scope
- Inner scope shadowing OK

### ADR-007: Aliasing allowed
- Multiple shared borrows OK
- Mutable borrow is exclusive
- No mutable + shared at same time

## Checker algorithm
```rust
impl BorrowChecker {
    pub fn check_function(&mut self, func: &Function) -> Result<(), Vec<BorrowError>>;
    
    fn check_expr(&mut self, expr: &Expr) -> Result<(), BorrowError>;
    fn check_move(&mut self, binding: BindingId, span: Span) -> Result<(), BorrowError>;
    fn check_borrow(&mut self, binding: BindingId, kind: BorrowKind, span: Span) -> Result<(), BorrowError>;
    fn end_borrow(&mut self, borrow: BorrowId);
    fn end_scope(&mut self, scope: ScopeId);
}
```

## Errors detected
- Use after move
- Move of borrowed value
- Mutable borrow while borrowed
- Double mutable borrow
- Borrow escapes scope
- Return reference to local

## Acceptance criteria
- [ ] Use-after-move detected
- [ ] Borrow rules enforced
- [ ] Mutable exclusivity enforced
- [ ] Borrow escape detected
- [ ] Drop insertion at scope end
- [ ] Good error messages with spans
