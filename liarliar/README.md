# liarliar - Self-hosted liar compiler

A bootstrap liar compiler written in liar itself. Compiles liar source to lIR.

## Status

**Working features:**
- Integer literals and arithmetic (`+`, `-`, `*`, `/`)
- Comparisons (`<`, `>`, `=`, `<=`, `>=`)
- Let bindings (single and multiple)
- If expressions with proper branching (`br`/`phi`) - recursion works!
- Recursive functions (countdown, sum-to-n, fibonacci, mutual recursion)
- Function definitions with typed parameters
- Function calls (direct, no closures)
- Return type annotations (`-> i64`)

**Known limitations:**
- No `ns` (namespace) support
- No `defstruct` 
- No `extern` (FFI)
- No closures (`fn`)
- No macros
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

```bash
./liarliar/tests/run-tests.sh
```

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
