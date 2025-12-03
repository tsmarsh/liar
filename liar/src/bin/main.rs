//! liarc - liar compiler binary

use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: liarc <file.liar>");
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
        // Compile file
        let path = &args[1];
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {}: {}", path, e);
                process::exit(1);
            }
        };

        match liar::compile(&source) {
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
