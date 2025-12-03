## Summary
Analyze closures for captures and color (ADR-010). Determine what
each closure captures and whether it's Send/Sync safe.

## Implementation

Implemented in `liar/src/closures.rs`:

### Capture analysis
```rust
pub struct CaptureInfo {
    pub captures: Vec<Capture>,
    pub color: ClosureColor,
}

pub struct Capture {
    pub name: String,
    pub binding: Option<BindingId>,
    pub mode: CaptureMode,
    pub span: Span,
}

pub enum CaptureMode {
    Move,       // Closure owns the value
    Borrow,     // Closure borrows (cannot escape)
    Clone,      // Closure owns a clone (future use)
}
```

### Closure color (ADR-010)
```rust
pub enum ClosureColor {
    Pure,       // No captures, can go anywhere
    Local,      // Captures borrows, cannot escape scope
    Sync,       // Captures only Send+Sync values, thread-safe
    NonSync,    // Captures non-Send values (future use)
}
```

Color propagation implemented in `ClosureColor::combine()`:
- Pure + anything = that thing
- Local + anything = Local
- Sync + Sync = Sync
- NonSync + anything = NonSync

### Analysis pass
```rust
pub struct ClosureAnalyzer {
    scope_stack: Vec<HashSet<String>>,
    closure_info: HashMap<Span, CaptureInfo>,
}
```

Key methods:
- `analyze()` - entry point, returns capture info for all closures
- `find_free_vars_for_lambda()` - identifies captured variables
- `determine_capture_mode()` - Move vs Borrow based on usage
- `is_used_as_borrow()` - detects (ref x) or (ref-mut x) usage
- `compute_color()` - derives color from capture modes

## Tests (all passing)
- `test_no_captures` - closure with no free vars is Pure
- `test_single_capture` - captures outer variable with Move
- `test_multiple_captures` - captures multiple variables
- `test_borrow_capture` - (ref x) usage results in Borrow mode
- `test_nested_closure` - detects both outer and inner closures
- `test_closure_color_pure` - no captures = Pure
- `test_closure_color_local` - borrow capture = Local
- `test_does_not_capture_params` - function params are captured
- `test_builtin_not_captured` - builtins like + not captured

## Acceptance criteria
- [x] Free variables identified
- [x] Capture mode determined correctly (Move vs Borrow)
- [x] Closure color computed
- [x] Borrow captures set color to Local (prevents escape)
- [x] Color propagates through nested closures
