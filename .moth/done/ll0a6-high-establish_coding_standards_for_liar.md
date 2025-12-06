## Summary

Establish and document coding standards to keep files manageable for agents.

## File Size Limits

| Metric | Limit | Action if exceeded |
|--------|-------|-------------------|
| Lines per file | 500 | Split into modules |
| Functions per file | 15 | Split by responsibility |
| Lines per function | 50 | Extract helpers |
| Nesting depth | 3 | Flatten or extract |

## Key Rules

### One Responsibility Per File
If you need section comments like `// ========== SECTION ==========` to navigate, split the file.

### No Thread-Local State for Pass Data
Bad:
```rust
thread_local! {
    static CAPTURED_VARS: RefCell<HashMap<...>> = ...
}
```

Good:
```rust
struct AnalysisContext {
    captured_vars: HashMap<...>,
}
```

### Passes Take and Return Data
Bad:
```rust
fn analyze(program: &Program) {
    // mutates global state
}
```

Good:
```rust
fn analyze(program: &Program) -> AnalysisResult {
    // returns result
}
```

### Explicit Dependencies
All functions take context explicitly, no hidden globals.

## Acceptance Criteria

- [ ] Standards documented in CONTRIBUTING.md or similar
- [ ] Team agrees on limits
- [ ] Apply during all subsequent moths
