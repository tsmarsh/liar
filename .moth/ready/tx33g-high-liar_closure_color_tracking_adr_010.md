## Summary
Track closure "color" to determine thread-safety. Colors propagate through
nesting and affect where closures can be used.

## Colors
```
Pure     - No captures, can go anywhere
Local    - Captures borrows, cannot escape scope
Sync     - Captures only Send+Sync values, thread-safe
NonSync  - Captures non-Send values, single-threaded only
```

## Color Rules
- Pure + anything = that thing
- Local + anything = Local
- Sync + Sync = Sync
- NonSync + anything = NonSync

## AST Additions
```rust
// In closures.rs
pub enum ClosureColor {
    Pure,
    Local,
    Sync,
    NonSync,
}

pub struct ClosureInfo {
    pub captures: Vec<Capture>,
    pub color: ClosureColor,
}
```

## Analysis
```rust
impl ClosureAnalyzer {
    fn compute_color(&self, captures: &[Capture]) -> ClosureColor {
        if captures.is_empty() {
            return ClosureColor::Pure;
        }
        
        let mut color = ClosureColor::Pure;
        for cap in captures {
            let cap_color = match cap.mode {
                CaptureMode::Borrow => ClosureColor::Local,
                CaptureMode::Move => {
                    if self.is_send_sync(&cap.ty) {
                        ClosureColor::Sync
                    } else {
                        ClosureColor::NonSync
                    }
                }
                CaptureMode::Clone => ClosureColor::Sync, // Cloned = owned copy
            };
            color = self.combine_colors(color, cap_color);
        }
        color
    }
}
```

## Verification
- plet body must be Sync or Pure
- async block must be Sync or Pure
- spawn requires Sync or Pure
- Local closures cannot escape defining scope

## Acceptance Criteria
- [ ] ClosureColor enum added
- [ ] Color computed for all closures
- [ ] Colors propagate through nesting
- [ ] plet rejects non-Sync closures
- [ ] Error messages indicate color violations
