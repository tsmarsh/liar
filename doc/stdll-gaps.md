# stdll smoke test gaps

## Scope
Quick check: can liarliar compile stdlib modules and can lair consume the emitted lIR.

## Commands run
- Build liarliar: `cargo run --release -p liar -- liarliar/main.liar > /tmp/liarliar.lir` and `./target/release/lair /tmp/liarliar.lir -o /tmp/liarliar`
- Smoke loop: `for f in lib/*.liar; do /tmp/liarliar "$f" > "/tmp/$(basename "$f" .liar).lir"; ./target/release/lair --compile-only "/tmp/$(basename "$f" .liar).lir" -o "/tmp/$(basename "$f" .liar).o"; done`

## Gaps found
1. **Undefined variables during lIR codegen**
   - `heap-array` in `array-clone` (`lib/liar.array.liar`).
   - `hash` in `hm-assoc`/`hs-add` (`lib/liar.hashmap.liar`, `lib/liar.hashset.liar`).
   - `len` in `mv-set!` (`lib/liar.mut-vector.liar`).
   - `write` in `write-blocking` (`lib/liar.runtime2.liar`).
   - `not` in `assert-not-nil` (`lib/liar.test.liar`).
   - `cons` in `vec-to-list` (`lib/liar.vector.liar`).

2. **Missing globals in lIR**
   - `undefined global or function: __lambda_0` when compiling `/tmp/liar.prelude.lir` and `/tmp/liar.seq.lir`.
   - Indicates a closure/function reference emitted without a matching definition.

3. **Unexpected tokens in lIR output**
   - `UnexpectedToken("+")` when compiling `/tmp/liar.runtime.core.lir`, `/tmp/liar.runtime.linux.lir`, `/tmp/liar.runtime.macos.lir`.
   - Suggests operator tokens leaking into places the lIR parser expects a list or symbol.

4. **Liarliar crashes or exits without diagnostics**
   - SIGSEGV when compiling `lib/liar.async.liar` and `lib/liar.io.liar`.
   - `lib/liar.runtime.liar` exits 1 with no stderr output.

## Notes
- Liarliar now prints empty param lists as `()` in lIR.
- Liarliar emits lIR for many stdlib files, but lair still fails to parse/assemble several outputs.
- The gaps above block progressing to linking/execution for the stdlib smoke test.
