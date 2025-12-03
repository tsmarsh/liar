## Summary
Create `lair` binary that compiles lIR source to native executables.
Codegen library stays pure (no I/O), lair handles files and linking.

## Architecture
```
lir-core      (lib)  AST, parser, types
lir-codegen   (lib)  AST → LLVM Module (pure, returns bytes)
lir-lair      (bin)  File I/O, linking, CLI
```

## CLI Interface

### Basic usage
```bash
lair main.lir -o prog              # compile + link
lair main.lir -c -o main.o         # compile only (no link)
lair main.lir utils.lir -o prog    # multiple inputs
```

### Optimization
```bash
lair main.lir -O0 -o prog          # no optimization (default)
lair main.lir -O1 -o prog          # basic
lair main.lir -O2 -o prog          # standard
lair main.lir -O3 -o prog          # aggressive
```

### Debug
```bash
lair main.lir -g -o prog           # emit DWARF debug info
lair main.lir --emit-llvm -o out.ll # emit LLVM IR text
lair main.lir --emit-bc -o out.bc  # emit LLVM bitcode
lair main.lir -S -o out.s          # emit assembly
```

### Linking
```bash
lair main.lir -lm -o prog          # link libm
lair main.lir -L/opt/lib -lfoo     # library search path
lair main.lir -static -o prog      # static linking
lair main.lir -shared -o libfoo.so # shared library
```

### Cross-compilation
```bash
lair main.lir --target aarch64-linux-gnu -o prog
lair --print-targets               # list available targets
```

### Other
```bash
lair main.lir -fPIC -o prog        # position independent code
lair main.lir --strip -o prog      # strip symbols
lair main.lir --entry start -o prog # custom entry point
lair main.lir -v -o prog           # verbose (show cc commands)
lair main.lir -Wl,-rpath,/opt/lib  # pass args to linker
```

## Implementation

### New crate: lir-lair
```
lir-lair/
  Cargo.toml
  src/
    main.rs      # CLI entry point
    args.rs      # Argument parsing (clap)
    compile.rs   # Orchestration logic
    link.rs      # Linker invocation
```

### Cargo.toml
```toml
[package]
name = "lir-lair"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "lair"
path = "src/main.rs"

[dependencies]
lir-core = { path = "../lir-core" }
lir-codegen = { path = "../lir-codegen" }
clap = { version = "4", features = ["derive"] }
tempfile = "3"
```

### args.rs (clap)
```rust
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "lair")]
#[command(about = "lIR assembler - compiles lIR to native code")]
pub struct Args {
    /// Input .lir files
    #[arg(required = true)]
    pub inputs: Vec<PathBuf>,

    /// Output file
    #[arg(short, long)]
    pub output: PathBuf,

    /// Compile only, don't link
    #[arg(short = 'c', long)]
    pub compile_only: bool,

    /// Optimization level
    #[arg(short = 'O', default_value = "0")]
    pub opt_level: u8,

    /// Emit debug info
    #[arg(short = 'g', long)]
    pub debug: bool,

    /// Emit LLVM IR instead of binary
    #[arg(long)]
    pub emit_llvm: bool,

    /// Emit LLVM bitcode instead of binary
    #[arg(long)]
    pub emit_bc: bool,

    /// Emit assembly instead of binary
    #[arg(short = 'S', long)]
    pub emit_asm: bool,

    /// Target triple for cross-compilation
    #[arg(long)]
    pub target: Option<String>,

    /// Print available targets
    #[arg(long)]
    pub print_targets: bool,

    /// Link libraries
    #[arg(short = 'l', action = clap::ArgAction::Append)]
    pub libs: Vec<String>,

    /// Library search paths
    #[arg(short = 'L', action = clap::ArgAction::Append)]
    pub lib_paths: Vec<PathBuf>,

    /// Build shared library
    #[arg(long)]
    pub shared: bool,

    /// Static linking
    #[arg(long = "static")]
    pub static_link: bool,

    /// Position independent code
    #[arg(long = "fPIC")]
    pub pic: bool,

    /// Strip symbols
    #[arg(long)]
    pub strip: bool,

    /// Entry point function name
    #[arg(long, default_value = "main")]
    pub entry: String,

    /// Verbose output
    #[arg(short = 'v', long)]
    pub verbose: bool,

    /// Pass arguments to linker
    #[arg(long = "Wl", value_delimiter = ',', action = clap::ArgAction::Append)]
    pub linker_args: Vec<String>,
}
```

### Codegen additions (pure, no I/O)
```rust
// In lir-codegen/src/lib.rs

pub enum OptLevel {
    None,       // -O0
    Less,       // -O1  
    Default,    // -O2
    Aggressive, // -O3
}

pub enum OutputFormat {
    Object,     // .o
    Assembly,   // .s
    LlvmIr,     // .ll
    Bitcode,    // .bc
}

impl CodeGen {
    /// Set optimization level
    pub fn set_opt_level(&mut self, level: OptLevel);
    
    /// Enable debug info generation
    pub fn enable_debug_info(&mut self);
    
    /// Set target triple (for cross-compilation)
    pub fn set_target(&mut self, triple: &str) -> Result<(), Error>;
    
    /// Emit output in specified format
    pub fn emit(&self, format: OutputFormat) -> Result<Vec<u8>, Error>;
    
    /// Get LLVM IR as string (convenience)
    pub fn emit_llvm_ir(&self) -> String;
    
    /// List available targets
    pub fn available_targets() -> Vec<String>;
}
```

### main.rs flow
```rust
fn main() -> Result<()> {
    let args = Args::parse();
    
    if args.print_targets {
        for target in CodeGen::available_targets() {
            println!("{}", target);
        }
        return Ok(());
    }
    
    // Parse all input files
    let mut items = Vec::new();
    for input in &args.inputs {
        let source = fs::read_to_string(input)?;
        let parsed = Parser::new(&source).parse_items()?;
        items.extend(parsed);
    }
    
    // Setup codegen
    let mut codegen = CodeGen::new();
    codegen.set_opt_level(match args.opt_level {
        0 => OptLevel::None,
        1 => OptLevel::Less,
        2 => OptLevel::Default,
        _ => OptLevel::Aggressive,
    });
    
    if args.debug {
        codegen.enable_debug_info();
    }
    
    if let Some(ref target) = args.target {
        codegen.set_target(target)?;
    }
    
    // Compile all items
    for item in &items {
        codegen.compile_item(item)?;
    }
    
    // Determine output format
    let format = if args.emit_llvm {
        OutputFormat::LlvmIr
    } else if args.emit_bc {
        OutputFormat::Bitcode
    } else if args.emit_asm {
        OutputFormat::Assembly
    } else {
        OutputFormat::Object
    };
    
    // Emit
    let bytes = codegen.emit(format)?;
    
    // If compile-only or non-object format, just write and done
    if args.compile_only || format != OutputFormat::Object {
        fs::write(&args.output, bytes)?;
        return Ok(());
    }
    
    // Write object to temp file, then link
    let temp_obj = tempfile::NamedTempFile::new()?;
    fs::write(temp_obj.path(), bytes)?;
    
    link(&args, temp_obj.path())?;
    
    Ok(())
}
```

### link.rs
```rust
fn link(args: &Args, object_path: &Path) -> Result<()> {
    let mut cmd = Command::new("cc");
    
    cmd.arg(object_path);
    cmd.arg("-o").arg(&args.output);
    
    if args.shared {
        cmd.arg("-shared");
    }
    
    if args.static_link {
        cmd.arg("-static");
    }
    
    if args.pic {
        cmd.arg("-fPIC");
    }
    
    if args.strip {
        cmd.arg("-s");
    }
    
    for lib in &args.libs {
        cmd.arg(format!("-l{}", lib));
    }
    
    for path in &args.lib_paths {
        cmd.arg(format!("-L{}", path.display()));
    }
    
    for arg in &args.linker_args {
        cmd.arg(format!("-Wl,{}", arg));
    }
    
    if args.verbose {
        eprintln!("{:?}", cmd);
    }
    
    let status = cmd.status()?;
    if !status.success() {
        return Err(Error::LinkFailed(status.code()));
    }
    
    Ok(())
}
```

## Testing

### Feature file: cert/features/lair.feature
```gherkin
Feature: lair AOT assembler

  Scenario: Compile and run hello world
    Given the file hello.lir containing a main that returns 42
    When I run lair hello.lir -o hello
    And I run ./hello
    Then the exit code is 42

  Scenario: Compile only
    Given the file test.lir
    When I run lair test.lir -c -o test.o
    Then test.o exists
    And test.o is an ELF object

  Scenario: Emit LLVM IR
    When I run lair test.lir --emit-llvm -o test.ll
    Then test.ll contains "define"

  Scenario: Optimization levels
    When I run lair test.lir -O3 -o test
    Then compilation succeeds

  Scenario: Link with library
    When I run lair math.lir -lm -o math
    Then compilation succeeds
```

## Acceptance criteria
- [ ] `lair` binary compiles and links lIR to native
- [ ] `-c` produces .o without linking
- [ ] `-O0/1/2/3` optimization levels work
- [ ] `-g` produces debug info
- [ ] `--emit-llvm`, `--emit-bc`, `-S` work
- [ ] `-l`, `-L` library linking works
- [ ] `--target` cross-compilation works
- [ ] `-shared` produces .so/.dylib
- [ ] `--strip` strips symbols
- [ ] Multiple input files work
- [ ] Exit codes: 0 success, non-zero failure
