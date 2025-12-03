## Summary
Analyze closures for captures and color (ADR-010). Determine what
each closure captures and whether it's Send/Sync safe.

## Capture analysis
```rust
pub struct CaptureInfo {
    pub captured: Vec<Capture>,
    pub color: ClosureColor,
}

pub struct Capture {
    pub binding: BindingId,
    pub mode: CaptureMode,
}

pub enum CaptureMode {
    Move,       // Closure owns the value
    Borrow,     // Closure borrows (cannot escape)
    Clone,      // Closure owns a clone
}
```

## Closure color (ADR-010)
```rust
pub enum ClosureColor {
    Pure,       // No captures, can go anywhere
    Local,      // Captures borrows, cannot escape scope
    Sync,       // Captures only Send+Sync values, thread-safe
    NonSync,    // Captures non-Send values, single-threaded only
}
```

Color propagation:
- Pure + anything = that thing
- Local + anything = Local
- Sync + Sync = Sync
- NonSync + anything = NonSync

## Analysis pass
```rust
pub struct ClosureAnalyzer {
    current_scope_bindings: HashSet<BindingId>,
}

impl ClosureAnalyzer {
    pub fn analyze_closure(&mut self, closure: &Fn, env: &Env) -> CaptureInfo;
    fn find_free_variables(&self, body: &[Expr]) -> Vec<BindingId>;
    fn determine_capture_mode(&self, binding: BindingId, usage: &Usage) -> CaptureMode;
    fn compute_color(&self, captures: &[Capture]) -> ClosureColor;
}
```

## Code generation implications
- Move capture: value moved into closure struct
- Borrow capture: closure has lifetime bound to scope
- Clone capture: clone called, closure owns copy
- Color affects where closure can be passed (spawn, async, etc.)

## Acceptance criteria
- [ ] Free variables identified
- [ ] Capture mode determined correctly
- [ ] Closure color computed
- [ ] Borrow captures prevent escape
- [ ] Color propagates through nested closures
