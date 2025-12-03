//! liar Interactive REPL
//!
//! Compiles liar to lIR, then JIT executes.
//!
//! Pipeline: liar source → liar compiler → lIR string → lIR parser → JIT

use inkwell::context::Context;
use lir_codegen::JitEngine;
use lir_core::parser::{ParseResult, Parser};
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

struct ReplState {
    /// Accumulated definitions (functions, structs, etc.) as liar source
    definitions: String,
}

impl ReplState {
    fn new() -> Self {
        Self {
            definitions: String::new(),
        }
    }

    /// Compile liar source to lIR string
    fn compile_to_lir(&self, input: &str) -> Result<String, String> {
        let full_source = format!("{}\n{}", self.definitions, input);
        liar::compile(&full_source).map_err(|errs| {
            errs.iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        })
    }

    /// Evaluate liar expression and return formatted result
    fn eval(&mut self, input: &str) -> Result<String, String> {
        // Check if this is a definition
        let trimmed = input.trim();
        if trimmed.starts_with("(defun")
            || trimmed.starts_with("(def ")
            || trimmed.starts_with("(defstruct")
            || trimmed.starts_with("(defprotocol")
        {
            // Accumulate definitions
            self.definitions.push_str(input);
            self.definitions.push('\n');
            return Ok("defined".to_string());
        }

        // Compile liar → lIR string
        let lir_str = self.compile_to_lir(input)?;

        // Parse lIR string → AST
        let mut parser = Parser::new(&lir_str);
        let items = parser
            .parse_items()
            .map_err(|e| format!("lIR parse error: {:?}", e))?;

        // Find expression to evaluate (last item that's an expression)
        let mut expr_to_eval = None;
        for item in items.iter().rev() {
            if let ParseResult::Expr(expr) = item {
                expr_to_eval = Some(expr.clone());
                break;
            }
        }

        let expr = expr_to_eval.ok_or_else(|| "No expression to evaluate".to_string())?;

        // JIT execute
        let context = Context::create();
        let jit = JitEngine::new(&context);
        let value = jit
            .eval(&expr)
            .map_err(|e| format!("Evaluation error: {}", e))?;

        Ok(format_result(&value))
    }
}

fn format_result(value: &lir_codegen::Value) -> String {
    use lir_codegen::Value;
    match value {
        Value::I1(b) => {
            if *b {
                "true"
            } else {
                "false"
            }
        }
        .to_string(),
        Value::I8(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I64(n) => n.to_string(),
        Value::Float(f) => format!("{}", f),
        Value::Double(d) => format!("{}", d),
        Value::Ptr(0) => "nil".to_string(),
        Value::Ptr(p) => format!("<ptr {:x}>", p),
        Value::VecI1(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|b| if *b { "true" } else { "false" })
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI8(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI16(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI32(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecI64(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecFloat(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::VecDouble(elems) => format!(
            "[{}]",
            elems
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Value::Struct(fields) => {
            let parts: Vec<String> = fields.iter().map(format_result).collect();
            format!("{{{}}}", parts.join(" "))
        }
    }
}

fn eval_line(state: &mut ReplState, line: &str) {
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
                    Ok(contents) => match state.eval(&contents) {
                        Ok(result) => println!("{}", result),
                        Err(e) => eprintln!("{}", e),
                    },
                    Err(e) => eprintln!("Error loading file: {}", e),
                }
            }
            ":lir" => {
                if parts.len() < 2 {
                    println!("Usage: :lir <expression>");
                    return;
                }
                match state.compile_to_lir(parts[1]) {
                    Ok(lir) => println!("{}", lir),
                    Err(e) => eprintln!("{}", e),
                }
            }
            _ => println!("Unknown command: {}. Type :help for help.", line),
        }
        return;
    }

    match state.eval(line) {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("{}", e),
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

    let mut state = ReplState::new();

    loop {
        let readline = rl.readline("liar> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                eval_line(&mut state, &line);
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
