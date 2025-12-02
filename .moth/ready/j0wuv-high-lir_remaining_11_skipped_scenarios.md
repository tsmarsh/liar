## Summary
After adding function call steps, 11 scenarios remain skipped. Audit and fix.

## Investigation needed
Run tests with verbose output to identify which scenarios are still skipped:
```bash
cargo test --test cert -- --nocapture 2>&1 | grep -A2 "skipped"
```

## Likely causes
1. Missing step definitions for edge cases:
   - `When I allocate X and call Y with Z`
   - `And I create an adder with captured value (i64 10)`
   - `When I call the adder with (i64 32)`
   
2. Complex integration_milestone scenarios that need special handling

3. Struct/GEP scenarios that need pointer setup

## Approach
1. List all 11 skipped scenarios
2. Check what step patterns they use
3. Add missing step definitions or fix feature file syntax

## Acceptance criteria
- [ ] All 11 skipped scenarios identified
- [ ] Missing step definitions added
- [ ] 201 passed, 0 skipped (after phi and type checker fixes)
