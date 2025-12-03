//! lair - lIR AOT assembler
//!
//! Compiles lIR source to native executables.

mod args;
mod link;

use anyhow::{bail, Context, Result};
use args::Args;
use clap::Parser;
use inkwell::context::Context as LlvmContext;
use lir_codegen::{CodeGen, OptLevel, OutputFormat};
use lir_core::parser::{ParseResult, Parser as LirParser};
use std::fs;
use std::io::Write;
use tempfile::NamedTempFile;

fn main() -> Result<()> {
    let args = Args::parse();

    // Handle --print-targets
    if args.print_targets {
        CodeGen::initialize_all_targets();
        for target in CodeGen::available_targets() {
            println!("{}", target);
        }
        return Ok(());
    }

    // Validate inputs
    if args.inputs.is_empty() {
        bail!("no input files");
    }

    // Determine output path
    let output = match &args.output {
        Some(p) => p.clone(),
        None => {
            // Default output name based on first input
            let stem = args.inputs[0]
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("a");

            if args.emit_llvm {
                format!("{}.ll", stem).into()
            } else if args.emit_bc {
                format!("{}.bc", stem).into()
            } else if args.emit_asm {
                format!("{}.s", stem).into()
            } else if args.compile_only {
                format!("{}.o", stem).into()
            } else {
                stem.into()
            }
        }
    };

    // Initialize LLVM target
    if args.target.is_some() {
        CodeGen::initialize_all_targets();
    } else {
        CodeGen::initialize_native_target();
    }

    // Parse all input files
    let mut items = Vec::new();
    for input in &args.inputs {
        let source = fs::read_to_string(input)
            .with_context(|| format!("failed to read {}", input.display()))?;
        let mut parser = LirParser::new(&source);
        let parsed = parser
            .parse_items()
            .map_err(|e| anyhow::anyhow!("parse error in {}: {:?}", input.display(), e))?;
        items.extend(parsed);
    }

    // Setup codegen
    let context = LlvmContext::create();
    let module_name = args.inputs[0]
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lir_module");
    let mut codegen = CodeGen::new(&context, module_name);

    // Set target triple if specified
    if let Some(ref triple) = args.target {
        codegen.set_target_triple(triple);
    }

    // Compile all items
    for item in &items {
        match item {
            ParseResult::Function(f) => {
                codegen.declare_function(f);
            }
            ParseResult::ExternDecl(decl) => {
                codegen
                    .compile_extern_decl(decl)
                    .map_err(|e| anyhow::anyhow!("codegen error: {:?}", e))?;
            }
            ParseResult::Struct(def) => {
                codegen.register_struct_type(&def.name, &def.fields);
            }
            ParseResult::Global(g) => {
                codegen
                    .compile_global(g)
                    .map_err(|e| anyhow::anyhow!("codegen error: {:?}", e))?;
            }
            ParseResult::Expr(_) => {
                // Skip top-level expressions in AOT mode
            }
        }
    }

    // Compile function bodies (second pass for mutual recursion)
    for item in &items {
        if let ParseResult::Function(f) = item {
            codegen
                .compile_function(f)
                .map_err(|e| anyhow::anyhow!("codegen error in {}: {:?}", f.name, e))?;
        }
    }

    // Determine optimization level
    let opt_level = match args.opt_level {
        0 => OptLevel::None,
        1 => OptLevel::Less,
        2 => OptLevel::Default,
        _ => OptLevel::Aggressive,
    };

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
    let bytes = codegen
        .emit(format, args.target.as_deref(), opt_level, args.pic)
        .map_err(|e| anyhow::anyhow!("emit error: {:?}", e))?;

    // If compile-only or non-object format, just write and done
    if args.compile_only || format != OutputFormat::Object {
        fs::write(&output, &bytes)
            .with_context(|| format!("failed to write {}", output.display()))?;

        if args.verbose {
            eprintln!("Wrote {} bytes to {}", bytes.len(), output.display());
        }
        return Ok(());
    }

    // Write object to temp file, then link
    let mut temp_obj = NamedTempFile::new().context("failed to create temp file")?;
    temp_obj.write_all(&bytes)?;
    temp_obj.flush()?;

    // Create a modified args with the resolved output path
    let mut link_args = args.clone();
    link_args.output = Some(output.clone());

    link::link(&link_args, temp_obj.path())?;

    if args.verbose {
        eprintln!("Linked {}", output.display());
    }

    Ok(())
}
