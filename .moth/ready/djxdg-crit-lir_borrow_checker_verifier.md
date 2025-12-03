## Summary
Implement the borrow checker as a verification pass over lIR.
Runs before codegen, rejects invalid programs.

## Verification Pass
```rust
pub struct BorrowChecker {
    bindings: HashMap<String, OwnershipState>,
    borrows: HashMap<String, BorrowInfo>,
    errors: Vec<BorrowError>,
}

pub enum OwnershipState {
    Owned,
    Moved,
    Borrowed { by: Vec<String> },
    BorrowedMut { by: String },
    Dropped,
}

pub struct BorrowInfo {
    kind: BorrowKind,
    source: String,      // What is borrowed
    scope_depth: usize,  // When borrow ends
}
```

## Rules Enforced

### Rule 1: No use after move
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((y x))          ; x moved to y
    (load i64 x)))      ; ERROR: x moved
```

### Rule 2: No use after drop
```lisp
; REJECT
(let ((x (alloc own i64)))
  (drop x)
  (load i64 x))         ; ERROR: x dropped
```

### Rule 3: Mutable borrow is exclusive
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((a (borrow refmut x))
        (b (borrow ref x)))   ; ERROR: x already mutably borrowed
    ...))
```

### Rule 4: Cannot borrow moved value
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((y x))          ; x moved
    (borrow ref x)))    ; ERROR: x moved
```

### Rule 5: Borrow cannot outlive owner
```lisp
; REJECT
(define (bad ref i64) ()
  (block entry
    (let ((x (alloc own i64)))
      (ret (borrow ref x)))))  ; ERROR: x dropped, borrow escapes
```

### Rule 6: All owned values must be dropped
```lisp
; REJECT (memory leak)
(define (leak void) ()
  (block entry
    (let ((x (alloc own i64)))
      (ret))))          ; ERROR: x not dropped
```

## Algorithm
```rust
impl BorrowChecker {
    pub fn check_function(&mut self, func: &Function) -> Result<(), Vec<BorrowError>> {
        // Initialize params
        for param in &func.params {
            match &param.ty {
                ParamType::Own(_) => self.bindings.insert(param.name.clone(), OwnershipState::Owned),
                ParamType::Ref(_) => self.bindings.insert(param.name.clone(), OwnershipState::Borrowed { by: vec![] }),
                // ...
            }
        }
        
        // Walk blocks in control flow order
        for block in &func.blocks {
            self.check_block(block)?;
        }
        
        // At function end, verify all owned dropped or returned
        self.verify_cleanup()?;
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    
    fn check_instruction(&mut self, inst: &Instruction) -> Result<(), BorrowError> {
        match inst {
            Instruction::Let { bindings, body } => {
                let scope_depth = self.enter_scope();
                for (name, expr) in bindings {
                    self.check_expr(expr)?;
                    // Track ownership based on expr result
                }
                for inst in body {
                    self.check_instruction(inst)?;
                }
                self.exit_scope(scope_depth)?;  // Verify borrows ended, insert drops
            }
            // ... other instructions
        }
    }
    
    fn check_use(&mut self, name: &str) -> Result<(), BorrowError> {
        match self.bindings.get(name) {
            Some(OwnershipState::Moved) => Err(BorrowError::UseAfterMove(name.to_string())),
            Some(OwnershipState::Dropped) => Err(BorrowError::UseAfterDrop(name.to_string())),
            Some(_) => Ok(()),
            None => Err(BorrowError::Undefined(name.to_string())),
        }
    }
}
```

## Error Messages
```
error: use of moved value `x`
  --> test.lir:5:10
   |
 3 |   (let ((y x))
   |            - value moved here
 4 |     ...
 5 |     (load i64 x))
   |              ^ value used here after move

error: cannot borrow `x` as immutable because it is already borrowed as mutable
  --> test.lir:4:20
   |
 3 |   (let ((a (borrow refmut x))
   |                           - mutable borrow occurs here
 4 |         (b (borrow ref x)))
   |                        ^ immutable borrow occurs here
```

## Test Cases
```gherkin
Scenario: Reject use after move
  Given the expression (define (bad void) () (block entry (let ((x (alloc own i64))) (let ((y x)) (load i64 x)) (ret))))
  Then verification fails with "use of moved value"

Scenario: Reject double mutable borrow
  Given the expression (define (bad void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow refmut x))) ...) (ret))))
  Then verification fails with "already borrowed as mutable"

Scenario: Accept valid borrows
  Given the expression (define (ok void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow ref x))) (add (load i64 a) (load i64 b))) (drop x) (ret))))
  Then verification succeeds
```

## Acceptance Criteria
- [ ] Use after move detected
- [ ] Use after drop detected
- [ ] Mutable exclusivity enforced
- [ ] Borrow escape detected
- [ ] Missing drop detected (optional: auto-insert)
- [ ] Good error messages with locations
- [ ] All valid programs pass
