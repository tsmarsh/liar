# stdll smoke test gaps

## Scope
Quick check: can liarliar compile stdlib modules and can lair consume the emitted lIR.

## Commands run
- `/tmp/liarliar lib/liar.core.liar > /tmp/liar_core.lir`
- `/tmp/liarliar lib/liar.seq.liar > /tmp/liar_seq.lir`
- `/tmp/liarliar lib/liar.hashmap.liar > /tmp/liar_hashmap.lir`
- `./target/release/lair /tmp/liar_core.lir -o /tmp/liar_core`
- `./target/release/lair /tmp/liar_seq.lir -o /tmp/liar_seq`
- `./target/release/lair /tmp/liar_hashmap.lir -o /tmp/liar_hashmap`

## Gaps found
1. **Zero-arg defun emits `nil` instead of `()` for params**
   - Lair parse error: `Expected ( found: nil`.
   - Seen in stdlib outputs (e.g., `/tmp/liar_seq.lir`, `/tmp/liar_hashmap.lir`).
   - Likely in `liarliar/codegen.liar` output for empty param lists.

2. **`/` operator becomes `indirect-call /` in emitted lIR**
   - Lair parse error: `UnexpectedToken("/")` when compiling `/tmp/liar_core.lir`.
   - The lIR contains `(indirect-call / ...)` instead of `sdiv`.
   - Suspected cause: `symbol-base-name` treats a bare `/` as namespaced, so it no longer matches the `str-slash` binop path.

## Notes
- Liarliar can emit lIR for stdlib files, but lair cannot parse the output yet.
- Fixing the two gaps above should allow the stdlib smoke test to proceed to linking/execution.
