//! liar Interactive REPL
//!
//! Uses incremental JIT for fast evaluation.

use inkwell::context::Context;
use liar_repl::session::{format_value, Session};
use liar_repl::EvalResult;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const BANNER: &str = r#"
liar REPL - A Lisp that doesn't lie about memory
Type expressions to evaluate, :help for commands, :quit to exit
"#;

fn print_help() {
    println!(
        r#"
Commands:
  :help, :h     Show this help
  :quit, :q     Exit the REPL
  :load FILE    Load and evaluate a file
  :lir EXPR     Show lIR output without evaluating

Examples:
  (+ 1 2)                ; => 3
  (defun square (x) (* x x))
  (square 5)             ; => 25
  (let ((x 10)) (* x x)) ; => 100
"#
    );
}

fn eval_line(session: &mut Session, line: &str) {
    let line = line.trim();
    if line.is_empty() {
        return;
    }

    // Handle commands
    if line.starts_with(':') {
        let parts: Vec<&str> = line.splitn(2, ' ').collect();
        match parts[0] {
            ":help" | ":h" => print_help(),
            ":quit" | ":q" => std::process::exit(0),
            ":load" => {
                if parts.len() < 2 {
                    println!("Usage: :load <file>");
                    return;
                }
                let path = parts[1].trim();
                match std::fs::read_to_string(path) {
                    Ok(contents) => {
                        // Evaluate each top-level form
                        for form in split_forms(&contents) {
                            match session.eval(&form) {
                                EvalResult::Value(v) => println!("{}", format_value(&v)),
                                EvalResult::Defined(n) => println!("defined: {}", n),
                                EvalResult::Error(e) => eprintln!("{}", e),
                            }
                        }
                    }
                    Err(e) => eprintln!("Error loading file: {}", e),
                }
            }
            ":lir" => {
                if parts.len() < 2 {
                    println!("Usage: :lir <expression>");
                    return;
                }
                // Just compile to lIR and show it
                match liar::compile_expr(parts[1]) {
                    Ok(lir) => println!("{}", lir),
                    Err(e) => eprintln!("{}", e),
                }
            }
            _ => println!("Unknown command: {}. Type :help for help.", line),
        }
        return;
    }

    match session.eval(line) {
        EvalResult::Value(v) => println!("{}", format_value(&v)),
        EvalResult::Defined(n) => println!("defined: {}", n),
        EvalResult::Error(e) => eprintln!("{}", e),
    }
}

/// Split source into individual top-level forms
fn split_forms(source: &str) -> Vec<String> {
    let mut forms = Vec::new();
    let mut current = String::new();
    let mut depth = 0;
    let mut in_string = false;
    let mut in_comment = false;
    let mut escape_next = false;

    for c in source.chars() {
        // Handle end of line comment
        if in_comment {
            if c == '\n' {
                in_comment = false;
            }
            continue;
        }

        if escape_next {
            current.push(c);
            escape_next = false;
            continue;
        }

        match c {
            ';' if !in_string => {
                in_comment = true;
            }
            '\\' if in_string => {
                current.push(c);
                escape_next = true;
            }
            '"' => {
                current.push(c);
                in_string = !in_string;
            }
            '(' if !in_string => {
                current.push(c);
                depth += 1;
            }
            ')' if !in_string => {
                current.push(c);
                depth -= 1;
                if depth == 0 && !current.trim().is_empty() {
                    forms.push(current.trim().to_string());
                    current.clear();
                }
            }
            _ => {
                if depth > 0 || !c.is_whitespace() {
                    current.push(c);
                }
            }
        }
    }

    // Add any remaining content (but filter out comments)
    let remaining = current.trim();
    if !remaining.is_empty() && !remaining.starts_with(';') {
        forms.push(remaining.to_string());
    }

    forms
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

    // Create LLVM context and session
    let context = Context::create();
    let mut session = match Session::new(&context, "repl".to_string()) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to initialize session: {}", e);
            std::process::exit(1);
        }
    };

    // Load stdlib if it exists
    let stdlib_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../lib/stdlib.liar");
    if let Ok(stdlib) = std::fs::read_to_string(stdlib_path) {
        for form in split_forms(&stdlib) {
            if let EvalResult::Error(e) = session.eval(&form) {
                eprintln!("Warning: stdlib load error: {}", e)
            }
        }
    }

    loop {
        let readline = rl.readline("liar> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                eval_line(&mut session, &line);
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
