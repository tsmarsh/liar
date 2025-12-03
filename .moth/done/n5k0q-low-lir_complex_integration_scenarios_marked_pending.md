## Summary
Some integration_milestone scenarios require complex test scaffolding
(allocating structs, calling closures) that are marked PENDING.

## Affected scenarios
- "When I allocate counter and call set-count with X"
- "And I create an adder with captured value X"  
- "When I call the adder with X"
- "Then loading field X returns Y"

## These require
1. Allocating memory from test harness
2. Passing pointers between test and JIT
3. Reading back struct fields

## Options
1. **Implement scaffolding** - test harness allocates memory, passes to JIT
2. **Rewrite scenarios** - make them self-contained in lIR code
3. **Accept as out of scope** - these test patterns liar won't need

## Recommendation
Option 2: Rewrite scenarios to be self-contained. The closure test should:
1. Call make_adder (returns ptr)
2. Call adder_fn with that ptr
3. Return the result

No external allocation needed - all memory management in lIR.

## Acceptance criteria
- [ ] Decide on approach
- [ ] Either implement scaffolding or rewrite scenarios
- [ ] 201 passed, 0 failed
