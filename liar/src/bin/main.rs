//! liarc - liar compiler binary

use liar::Target;
use std::env;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: liarc <file.liar> [--lib <path>]... [--target <platform>]");
        eprintln!("       liarc -e <expression>");
        eprintln!();
        eprintln!("Targets: linux, macos, windows, wasi");
        process::exit(1);
    }

    if args[1] == "-e" {
        // Compile expression
        if args.len() < 3 {
            eprintln!("Usage: liarc -e <expression>");
            process::exit(1);
        }
        let expr = &args[2];
        match liar::compile_expr(expr) {
            Ok(lir) => println!("{}", lir),
            Err(e) => {
                eprintln!("Error: {}", e);
                process::exit(1);
            }
        }
    } else {
        // Compile file with optional library paths and target
        let path = &args[1];

        // Parse options
        let (lib_paths, target) = parse_options(&args[2..]);

        // Compile with target-aware module resolution
        let result = liar::loader::compile_file_with_target(Path::new(path), &lib_paths, target);

        match result {
            Ok(lir) => print!("{}", lir),
            Err(errors) => {
                for e in errors {
                    eprintln!("Error: {}", e);
                }
                process::exit(1);
            }
        }
    }
}

/// Parse --lib and --target arguments from command line
fn parse_options(args: &[String]) -> (Vec<PathBuf>, Target) {
    let mut lib_paths = Vec::new();
    let mut target = Target::host();
    let mut i = 0;

    while i < args.len() {
        if args[i] == "--lib" {
            if i + 1 < args.len() {
                lib_paths.push(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!("Error: --lib requires a path argument");
                process::exit(1);
            }
        } else if args[i] == "--target" {
            if i + 1 < args.len() {
                target = Target::parse(&args[i + 1]).unwrap_or_else(|| {
                    eprintln!(
                        "Error: unknown target '{}', expected: linux, macos, windows, wasi",
                        &args[i + 1]
                    );
                    process::exit(1);
                });
                i += 2;
            } else {
                eprintln!("Error: --target requires a platform argument");
                process::exit(1);
            }
        } else {
            i += 1;
        }
    }

    // Add default lib path if it exists
    let default_lib = PathBuf::from("lib");
    if default_lib.exists() && !lib_paths.contains(&default_lib) {
        lib_paths.push(default_lib);
    }

    (lib_paths, target)
}
