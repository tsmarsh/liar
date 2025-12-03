## Summary
Create the liar compiler CLI that compiles .liar to .lir.

## Usage
```bash
# Compile to lIR (stdout)
liarc input.liar

# Compile to lIR file
liarc input.liar -o output.lir

# Compile and assemble (requires lair)
liarc input.liar --emit=obj -o output.o
liarc input.liar --emit=exe -o program

# Check only (no output)
liarc input.liar --check

# Verbose (show passes)
liarc input.liar -v
```

## Options
```rust
#[derive(Parser)]
pub struct Args {
    /// Input .liar file
    pub input: PathBuf,
    
    /// Output file (default: stdout)
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    
    /// Output format
    #[arg(long, default_value = "lir")]
    pub emit: Emit,
    
    /// Type check only, no codegen
    #[arg(long)]
    pub check: bool,
    
    /// Verbose output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Dump AST
    #[arg(long)]
    pub dump_ast: bool,
    
    /// Dump typed AST
    #[arg(long)]
    pub dump_typed: bool,
}

pub enum Emit {
    Lir,    // .lir text
    Obj,    // .o (via lair)
    Exe,    // executable (via lair)
}
```

## Error output
Errors should be human-readable with source locations:

```
error[E0382]: use of moved value: `x`
 --> input.liar:10:5
  |
8 |     (consume x)
  |              - value moved here
9 |     ...
10|     x
  |     ^ value used here after move
  |
  = note: move occurs because `x` has type `Pair`, which does not implement `Copy`
```

## Acceptance criteria
- [ ] Compiles .liar to .lir
- [ ] --check validates without output
- [ ] Error messages have source locations
- [ ] --verbose shows pipeline stages
- [ ] Exit code 0 on success, 1 on error
