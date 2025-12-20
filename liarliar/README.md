# liarliar - Self-hosted liar compiler

A bootstrap liar compiler written in liar itself. Compiles liar source to lIR.

## Status

**Working features:**
- Integer literals and arithmetic (`+`, `-`, `*`, `/`, `rem`)
- Float literals and arithmetic (`+.`, `-.`, `*.`, `/.`, `%.`, `fadd`, `fsub`, `fmul`, `fdiv`, `frem`)
- Type conversions (`trunc`, `zext`, `sext`, `fptosi`, `fptoui`, `sitofp`, `uitofp`, `fpext`, `fptrunc`)
- Comparisons (`<`, `>`, `=`, `<=`, `>=`)
- Let bindings (single and multiple)
- If expressions with proper branching (`br`/`phi`) - recursion works
- Tail calls (`tailcall`/`indirect-tailcall`) in codegen
- Recursive functions (countdown, sum-to-n, fibonacci, mutual recursion)
- Function definitions with typed parameters
- Function calls (direct and indirect)
- Structs (`defstruct`, constructor, field access)
- `share`, `nil?`, `instance?`
- Protocols (`defprotocol`, `extend-protocol`, default impls, generic protocol dispatch)
- Macros with quasiquote, unquote (`,` and `~`), unquote-splicing (`~@`)
- Nested macro expansion

**Known limitations:**
- No `ns` (namespace) support
- No `extern` (FFI)
- Closure captures are incomplete (captured callables can fail)
- Macro expansion gaps: variadic macros (`...` rest), `thread-first`/`thread-last`, and `assoc`-style struct updates
- Some tailcall paths still require block context

## Building

```bash
# Build the Rust liar compiler
cargo build --release -p liar

# Compile liarliar to native
cargo run --release -p liar -- liarliar/main.liar > /tmp/liarliar.lir
./target/release/lair /tmp/liarliar.lir -o /tmp/liarliar
```

## Testing

### Unit Tests

```bash
LIARLIAR=/tmp/liarliar ./liarliar/tests/run-tests.sh
```

### Certification Tests

Run the full liar certification suite against liarliar to measure progress toward bootstrap:

```bash
# Build liarliar first
cargo run --release -p liar -- liarliar/main.liar 2>/dev/null > /tmp/liarliar.lir
./target/release/lair /tmp/liarliar.lir -o /tmp/liarliar

# Run cert tests with liarliar backend
USE_LIARLIAR=1 cargo test --release -p liar-cert --test cert
```

This runs all 118 scenarios from `liar-cert/features/*.feature` against liarliar instead of the Rust compiler. Currently **97 scenarios pass** and **21 fail**; the remaining failures cluster around macro expansion gaps and closure capture issues.

**Note:** Float tests require return type inference or explicit annotations. Since the test harness uses exit codes (integers), float return values don't work yet.

Without `USE_LIARLIAR`, the same tests run against the Rust liar compiler (all 118 pass).

## Usage

```bash
# Compile a liar file to lIR
/tmp/liarliar input.liar > output.lir

# Or from stdin
echo '(defun main () -> i64 42)' | /tmp/liarliar
```

## Next Steps

1. **Macro expansion**: variadic macros, thread-first/last, assoc helpers
2. **Closure capture**: fix captured callables in `fn`
3. **Tailcall contexts**: allow tailcall outside block-only paths
4. **FFI**: `extern` declarations
5. **Namespace support**: handle `ns` declarations
