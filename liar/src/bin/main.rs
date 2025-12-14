//! liarc - liar compiler binary

use std::env;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: liarc <file.liar> [--lib <path>]...");
        eprintln!("       liarc -e <expression>");
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
        // Compile file with optional library paths
        let path = &args[1];

        // Parse --lib options
        let lib_paths = parse_lib_paths(&args[2..]);

        // Check if the file uses namespaces by trying to compile with module resolution
        let result = liar::compile_file(Path::new(path), &lib_paths);

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

/// Parse --lib arguments from command line
fn parse_lib_paths(args: &[String]) -> Vec<PathBuf> {
    let mut lib_paths = Vec::new();
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
        } else {
            i += 1;
        }
    }

    // Add default lib path if it exists
    let default_lib = PathBuf::from("lib");
    if default_lib.exists() && !lib_paths.contains(&default_lib) {
        lib_paths.push(default_lib);
    }

    lib_paths
}
