# Unified Cert Test Runner for liar and liarliar

## Summary

Create a test runner that executes the same `.feature` files against both the Rust liar compiler and the self-hosted liarliar compiler, providing evidence of behavioral equivalence as liarliar matures toward bootstrap.

## Motivation

The `liar-cert/features/` directory contains 15 feature files with ~117 scenarios that specify liar's behavior. Currently these only run against the Rust compiler. As liarliar gains capabilities, we need to:

1. **Verify equivalence** — Same input produces same output from both compilers
2. **Track progress** — See which scenarios liarliar passes vs. the Rust compiler
3. **Catch regressions** — Ensure liarliar doesn't diverge from spec
4. **Build confidence** — Passing the full cert suite is strong evidence liarliar is ready for self-hosting

## Design

### Approach: Shared Features, Dual Backends

```
liar-cert/features/*.feature
         │
         ├──→ Rust runner (existing) ──→ liar ──→ lIR ──→ JIT ──→ result
         │
         └──→ liarliar runner (new) ──→ liarliar ──→ lIR ──→ lair ──→ result
```

Both runners parse the same Gherkin features and compare results. The liarliar runner can mark scenarios as "pending" when they use features liarliar doesn't support yet.

### Runner Architecture

Create `liarliar-cert/` as a lightweight shell/Python runner (not Rust, to avoid bootstrap complexity):

```
liarliar-cert/
├── run-cert.sh           # Main entry point
├── parse-feature.py      # Gherkin parser (simple, no cucumber dep)
├── results/              # Test output
│   ├── rust-results.json
│   └── liarliar-results.json
└── compare.py            # Diff the two result sets
```

### Gherkin Parsing

The feature file format is simple enough to parse without a full Gherkin library:

```gherkin
Feature: Core Language Tests

  Scenario: Simple addition
    Given the definition (defun test () (+ 1 2))
    When I evaluate (test)
    Then the result is 3
```

Parser extracts:
- Scenario name
- All `Given the definition (...)` lines → concatenate as source
- `When I evaluate (...)` → function to call (always `test` by convention)
- `Then the result is X` → expected value

### Test Execution

For each scenario:

```bash
# Rust path (reference)
echo "$SOURCE" | cargo run -p liar -- - > /tmp/test.lir
./target/release/lair /tmp/test.lir -o /tmp/test
RUST_RESULT=$(/tmp/test; echo $?)

# liarliar path
echo "$SOURCE" | /tmp/liarliar > /tmp/test-ll.lir
./target/release/lair /tmp/test-ll.lir -o /tmp/test-ll
LIARLIAR_RESULT=$(/tmp/test-ll; echo $?)

# Compare
if [ "$RUST_RESULT" = "$LIARLIAR_RESULT" ]; then
  echo "PASS: $SCENARIO"
else
  echo "FAIL: $SCENARIO (rust=$RUST_RESULT, liarliar=$LIARLIAR_RESULT)"
fi
```

### Handling Unsupported Features

liarliar doesn't support everything yet. The runner should:

1. **Detect unsupported constructs** — If source contains `fn`, `defstruct`, `defprotocol`, etc. that liarliar can't handle, mark as SKIP
2. **Catch crashes** — If liarliar exits non-zero on compile, mark as ERROR
3. **Distinguish from failures** — SKIP/ERROR are different from FAIL (wrong result)

```
Status meanings:
  PASS  - Both compilers produce same result
  FAIL  - Both compile, but results differ
  SKIP  - Source uses features liarliar doesn't support
  ERROR - liarliar crashed or produced invalid lIR
  RUST_ONLY - Rust passes, liarliar not attempted (explicit skip list)
```

### Feature Support Detection

Create a simple heuristic based on source content:

```python
UNSUPPORTED_PATTERNS = [
    (r'\(fn\s', 'closures'),
    (r'\(defstruct\s', 'structs'),
    (r'\(defprotocol\s', 'protocols'),
    (r'\(extend-protocol\s', 'protocols'),
    (r'\(defmacro\s', 'macros'),  # liarliar has expand.liar but may not be wired
    (r'\(async\s', 'async'),
    (r'\(await\s', 'async'),
    # Add more as needed
]

def check_supported(source):
    for pattern, feature in UNSUPPORTED_PATTERNS:
        if re.search(pattern, source):
            return False, feature
    return True, None
```

### Output Format

JSON for easy diffing and CI integration:

```json
{
  "runner": "liarliar",
  "timestamp": "2024-01-15T10:30:00Z",
  "summary": {
    "total": 117,
    "pass": 42,
    "fail": 3,
    "skip": 70,
    "error": 2
  },
  "scenarios": [
    {
      "feature": "simple.feature",
      "scenario": "Simple addition",
      "status": "PASS",
      "expected": 3,
      "actual": 3
    },
    {
      "feature": "tailcall.feature",
      "scenario": "Tail recursive countdown",
      "status": "ERROR",
      "error": "liarliar crashed: stack overflow"
    },
    {
      "feature": "closures.feature",
      "scenario": "Closure returning ptr",
      "status": "SKIP",
      "reason": "closures not supported"
    }
  ]
}
```

### Comparison Report

```
=== liar vs liarliar Certification Report ===

Feature                  Rust    liarliar  Match
─────────────────────────────────────────────────
simple.feature           8/8     8/8       ✓ 100%
closures.feature         1/1     0/1 SKIP  - 
macros.feature           9/9     0/9 SKIP  -
structs.feature          4/4     0/4 SKIP  -
tailcall.feature         6/6     0/6 ERROR ✗
...

Total: Rust 117/117, liarliar 42/117 (35.9% equivalent)

Blockers for full equivalence:
  - closures: 1 scenario
  - structs: 4 scenarios  
  - protocols: 12 scenarios
  - branching (ERROR): 6 scenarios  ← fix this first!
```

## Implementation Plan

### Phase 1: Basic Runner

1. Create `liarliar-cert/` directory structure
2. Write `parse-feature.py` to extract scenarios from `.feature` files
3. Write `run-cert.sh` that:
   - Builds liarliar if needed
   - Iterates scenarios
   - Runs each through liarliar → lair → execute
   - Captures exit code as result
   - Outputs JSON results

### Phase 2: Dual Comparison

1. Add Rust compiler path to runner (or call existing `liar-cert` and capture output)
2. Write `compare.py` to diff result sets
3. Generate comparison report

### Phase 3: CI Integration

1. Add GitHub Action that runs both test suites
2. Fail CI if liarliar regresses (passes fewer scenarios than before)
3. Track progress over time with badge/metrics

### Phase 4: Feature Detection

1. Implement `UNSUPPORTED_PATTERNS` detection
2. Auto-skip scenarios using unsupported features
3. Update skip list as liarliar gains capabilities

## File Structure

```
liarliar-cert/
├── run-cert.sh              # ./run-cert.sh [--rust|--liarliar|--both]
├── parse_feature.py         # Gherkin → JSON scenarios
├── run_scenario.sh          # Execute single scenario
├── compare.py               # Diff two result sets
├── unsupported.txt          # Explicit skip list (optional)
└── results/
    ├── .gitignore
    └── (generated JSON files)
```

## Example Usage

```bash
# Run just liarliar
./liarliar-cert/run-cert.sh --liarliar

# Run both and compare
./liarliar-cert/run-cert.sh --both

# Run specific feature
./liarliar-cert/run-cert.sh --liarliar --feature simple.feature

# Compare previous results
./liarliar-cert/compare.py results/rust-results.json results/liarliar-results.json
```

## Acceptance Criteria

- [ ] `parse_feature.py` correctly extracts all scenarios from all `.feature` files
- [ ] `run-cert.sh --liarliar` executes scenarios and produces JSON output
- [ ] `run-cert.sh --rust` executes scenarios via Rust liar (or reuses existing results)
- [ ] `run-cert.sh --both` runs both and calls `compare.py`
- [ ] `compare.py` produces readable diff report
- [ ] Runner correctly marks SKIP for unsupported features
- [ ] Runner correctly marks ERROR for crashes
- [ ] All currently-passing liarliar tests (from `liarliar/tests/run-tests.sh`) also pass in cert runner
- [ ] CI integration tracks pass rate over time

## Dependencies

- Python 3.x (for parsing/comparison scripts)
- Existing `lair` binary (for lIR → native)
- Built `liarliar` binary

No external Python packages required — just stdlib `re`, `json`, `subprocess`.

## Relationship to Other Moths

**Depends on:** Nothing (can run with current liarliar capabilities)

**Blocked by:** Nothing

**Enables:**
- `001-br4nc` (branching) — will unblock `tailcall.feature` scenarios
- Future work — provides ongoing validation as features are added

## Notes

The key insight is that both compilers target the same lIR, so we can compare at the "final result" level without needing to compare intermediate representations. If both produce executables that return the same value, they're behaviorally equivalent for that scenario.

This gives us a concrete, measurable definition of "liarliar is ready for bootstrap": when it passes 100% of the scenarios that don't require features intentionally omitted from the bootstrap compiler (like async, which isn't needed for self-hosting).

## Success Metric

**Bootstrap readiness threshold:** liarliar passes all scenarios in:
- `simple.feature` (core arithmetic, let bindings)
- `tailcall.feature` (recursion — requires branching fix)
- `structs.feature` (struct definition and field access)
- `macros.feature` (macro expansion)

That subset covers what's needed to compile a compiler. Closures, protocols, async, etc. can come later.
