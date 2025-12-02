## Summary
Run clippy and fix any warnings. Remove dead code, unused imports, etc.

## Commands
```bash
cargo clippy --all-targets --all-features -- -D warnings
cargo fmt --all -- --check
```

## Common issues to fix
- `#[allow(dead_code)]` annotations that can be removed
- Unused imports
- Redundant clones
- Missing docs on public items

## Acceptance Criteria
- [ ] `cargo clippy` passes with no warnings
- [ ] `cargo fmt` passes
- [ ] No `#[allow(dead_code)]` unless justified
