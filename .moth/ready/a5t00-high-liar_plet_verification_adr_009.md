## Summary
plet is a compile-time construct that marks closures as thread-safe.
It executes sequentially like let, but enforces that only atoms are
used for mutable state. This enables passing plet-closures to pmap/pfilter.

## Semantics
```lisp
; plet marks closure as Sync (thread-safe)
; Mutable state must use atoms
; Non-atom bindings are treated as constants

(plet ((cache (atom []))    ; mutable — atom required
       (multiplier 10))     ; immutable — constant
  (fn (x) 
    (swap! cache ...)       ; OK: atom
    (set! multiplier 20)))  ; ERROR: not an atom
```

## Verification Rules

### In plet body:
1. `set!` on non-atom binding = ERROR
2. Mutation through non-atom reference = ERROR
3. Capturing non-atom mutable state = ERROR

### Closure color:
- let-closure with mutable captures → NonSync
- plet-closure → Sync (verified safe)

## Implementation
```rust
// In ownership.rs or new plet_check.rs

pub struct PletChecker {
    atom_bindings: HashSet<String>,
    errors: Vec<PletError>,
}

impl PletChecker {
    pub fn check_plet(&mut self, bindings: &[LetBinding], body: &Expr) {
        // Identify which bindings are atoms
        for binding in bindings {
            if self.is_atom_expr(&binding.value) {
                self.atom_bindings.insert(binding.name.clone());
            }
        }
        
        // Check body for non-atom mutations
        self.check_mutations(body);
    }
    
    fn check_mutations(&mut self, expr: &Expr) {
        match expr {
            Expr::Set(name, _) => {
                if !self.atom_bindings.contains(&name.value) {
                    self.errors.push(PletError::NonAtomMutation(name.clone()));
                }
            }
            // ... recurse into sub-expressions
        }
    }
}
```

## Error Messages
```
error: cannot mutate non-atom binding in plet
  --> src/main.liar:10:5
   |
 8 | (plet ((counter 0))
   |        ------- binding is not an atom
 9 |   (fn ()
10 |     (set! counter (+ counter 1))))
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ mutation requires atom
   |
   = help: use (atom 0) instead of 0
```

## Codegen
plet compiles identically to let — the safety is compile-time only:
```lisp
; (plet ((x (atom 0))) body)
; compiles to same as:
; (let ((x (atom 0))) body)
```

## Integration with pmap
```lisp
; let-closure: NonSync color
(let ((cache []))
  (fn (x) (append cache x)))

(pmap let-closure data)  ; ERROR: NonSync closure to parallel op

; plet-closure: Sync color
(plet ((cache (atom [])))
  (fn (x) (swap! cache (fn (c) (append c x)))))

(pmap plet-closure data)  ; OK: Sync closure
```

## Acceptance Criteria
- [ ] plet parses (already done)
- [ ] Non-atom mutation in plet body = error
- [ ] plet closures marked as Sync color
- [ ] let closures with mutable captures marked NonSync
- [ ] pmap rejects NonSync closures
- [ ] Good error messages with suggestions
