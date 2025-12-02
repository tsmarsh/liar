//! lIR Command-line evaluator
//!
//! Evaluates a single lIR expression and prints the result.
//! Exit code 0 on success, non-zero on error.

use inkwell::context::Context;
use lir_codegen::JitEngine;
use lir_core::{parser::Parser, types::TypeChecker};
use std::env;
use std::io::{self, Read};

fn eval(input: &str) -> Result<String, String> {
    // Parse
    let mut parser = Parser::new(input);
    let expr = parser.parse().map_err(|e| format!("parse error: {}", e))?;

    // Type check
    let checker = TypeChecker::new();
    checker
        .check(&expr)
        .map_err(|e| format!("type error: {}", e))?;

    // JIT execute
    let context = Context::create();
    let jit = JitEngine::new(&context);
    let value = jit
        .eval(&expr)
        .map_err(|e| format!("evaluation error: {}", e))?;

    Ok(format!("{}", value))
}

fn main() {
    // Get expression from args or stdin
    let args: Vec<String> = env::args().collect();

    let input = if args.len() > 1 {
        // Expression provided as argument
        args[1..].join(" ")
    } else {
        // Read from stdin
        let mut buffer = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut buffer) {
            eprintln!("error reading stdin: {}", e);
            std::process::exit(1);
        }
        buffer
    };

    let input = input.trim();
    if input.is_empty() {
        eprintln!("usage: lir <expression>");
        eprintln!("   or: echo '<expression>' | lir");
        std::process::exit(1);
    }

    match eval(input) {
        Ok(result) => {
            println!("{}", result);
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
