use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug, Clone)]
#[command(name = "lair")]
#[command(about = "lIR assembler - compiles lIR to native code")]
#[command(version)]
pub struct Args {
    /// Input .lir files
    #[arg(required_unless_present = "print_targets")]
    pub inputs: Vec<PathBuf>,

    /// Output file
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Compile only, don't link
    #[arg(short = 'c', long)]
    pub compile_only: bool,

    /// Optimization level (0-3)
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

    /// Link libraries (-l<lib>)
    #[arg(short = 'l', action = clap::ArgAction::Append)]
    pub libs: Vec<String>,

    /// Library search paths (-L<path>)
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

    /// Pass arguments to linker (-Wl,<arg>)
    #[arg(long = "Wl", value_delimiter = ',', action = clap::ArgAction::Append)]
    pub linker_args: Vec<String>,
}
