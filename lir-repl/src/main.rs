//! lIR Interactive REPL
//!
//! A JIT-powered REPL for the lIR language.

use inkwell::context::Context;
use lir_codegen::JitEngine;
use lir_core::{parser::Parser, types::TypeChecker};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const BANNER: &str = r#"
lIR REPL - LLVM IR in S-expressions
Type expressions to evaluate, :help for commands, :quit to exit
"#;

fn print_help() {
    println!(
        r#"
Commands:
  :help, :h     Show this help
  :quit, :q     Exit the REPL

Examples:
  (i32 42)                          ; Integer literal
  (add (i32 5) (i32 7))             ; Integer addition
  (fadd (double 5.0) (double 6.0))  ; Float addition
  (icmp eq (i32 5) (i32 5))         ; Integer comparison
  (select (i1 1) (i32 10) (i32 20)) ; Conditional select
"#
    );
}

fn eval_line(context: &Context, line: &str) {
    // Trim and check for empty input
    let line = line.trim();
    if line.is_empty() {
        return;
    }

    // Handle commands
    if line.starts_with(':') {
        match line {
            ":help" | ":h" => print_help(),
            ":quit" | ":q" => std::process::exit(0),
            _ => println!("Unknown command: {}. Type :help for help.", line),
        }
        return;
    }

    // Parse
    let mut parser = Parser::new(line);
    let expr = match parser.parse() {
        Ok(e) => e,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    // Type check
    let mut checker = TypeChecker::new();
    if let Err(e) = checker.check(&expr) {
        eprintln!("Type error: {}", e);
        return;
    }

    // JIT execute
    let jit = JitEngine::new(context);
    match jit.eval(&expr) {
        Ok(value) => println!("{}", value),
        Err(e) => eprintln!("Evaluation error: {}", e),
    }
}

fn main() {
    println!("{}", BANNER);

    let mut rl = match DefaultEditor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize editor: {}", e);
            std::process::exit(1);
        }
    };

    // Create LLVM context once
    let context = Context::create();

    loop {
        let readline = rl.readline("lir> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                eval_line(&context, &line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
