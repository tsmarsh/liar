# liarliar - Self-hosted liar compiler

A bootstrap liar compiler written in liar itself. Compiles liar source to lIR.

## Status

**Working features:**
- Integer literals and arithmetic (`+`, `-`, `*`, `/`, `rem`)
- Float literals and arithmetic (`+.`, `-.`, `*.`, `/.`, `%.`, `fadd`, `fsub`, `fmul`, `fdiv`, `frem`)
- Type conversions (`trunc`, `zext`, `sext`, `fptosi`, `fptoui`, `sitofp`, `uitofp`, `fpext`, `fptrunc`)
- Comparisons (`<`, `>`, `=`, `<=`, `>=`)
- Let bindings (single and multiple)
- If expressions with proper branching (`br`/`phi`) - recursion works!
- Recursive functions (countdown, sum-to-n, fibonacci, mutual recursion)
- Function definitions with typed parameters
- Function calls (direct, no closures)
- Return type annotations (`-> i64`)
- Macros with quasiquote, unquote (`,` and `~`), unquote-splicing (`~@`)
- Nested macro expansion

**Known limitations:**
- No `ns` (namespace) support
- No `defstruct`
- No `extern` (FFI)
- No closures (`fn`)
- No variadic macros (`...` rest parameters)
- No field access (`.`)
- No `instance?`, `share`, `nil?`
- Only `i64` return type supported

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

This runs all 118 scenarios from `liar-cert/features/*.feature` against liarliar instead of the Rust compiler. Currently **29 scenarios pass** (basic arithmetic, type conversions, let bindings, macros with quasiquote/unquote). As liarliar gains features, more scenarios will pass.

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

1. **Namespace support**: Handle `ns` declarations
2. **Struct support**: `defstruct`, field access, `instance?`
3. **FFI**: `extern` declarations
4. **Self-compilation**: Compile liarliar with itself
