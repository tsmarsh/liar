//! liar-lint - lightweight linter for liar source

use std::env;
use std::fs;
use std::process;

use liar::lint::{lint_source, LintWarning};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: liar-lint <file.liar> [file2.liar ...]");
        process::exit(1);
    }

    let mut had_error = false;
    for path in &args[1..] {
        match fs::read_to_string(path) {
            Ok(source) => match lint_source(&source) {
                Ok(warnings) => {
                    for warn in warnings {
                        print_warning(path, &source, &warn);
                    }
                }
                Err(err) => {
                    had_error = true;
                    eprintln!("{}: {}", path, err);
                }
            },
            Err(err) => {
                had_error = true;
                eprintln!("{}: failed to read file: {}", path, err);
            }
        }
    }

    if had_error {
        process::exit(1);
    }
}

fn print_warning(path: &str, source: &str, warn: &LintWarning) {
    let (line, col) = byte_offset_to_line_col(source, warn.span.start);
    eprintln!(
        "{}:{}:{}: lint {}: {}",
        path, line, col, warn.kind, warn.message
    );
}

fn byte_offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}
