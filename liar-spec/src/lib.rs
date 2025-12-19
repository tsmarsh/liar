//! liar-spec: Compilation specification tests
//!
//! Verifies that liar source compiles to the expected lIR output.
//! This binds the language definition to its compilation target.

use std::path::Path;
use std::process::Command;

/// Compile liar source to lIR using the Rust liar compiler
pub fn compile_to_lir(source: &str) -> Result<String, String> {
    // Write source to temp file
    let temp_path = "/tmp/liar_spec_test.liar";
    std::fs::write(temp_path, source).map_err(|e| format!("Failed to write temp file: {}", e))?;

    // Find liarc - try multiple locations
    let liarc_paths = [
        "target/release/liarc",
        "./target/release/liarc",
        "../target/release/liarc",
        "../../target/release/liarc",
    ];

    let liarc = liarc_paths
        .iter()
        .find(|p| Path::new(p).exists())
        .ok_or_else(|| "Could not find liarc binary. Run `cargo build --release -p liar` first.".to_string())?;

    // Run liarc
    let output = Command::new(liarc)
        .arg(temp_path)
        .output()
        .map_err(|e| format!("Failed to run liarc: {}", e))?;

    if output.status.success() {
        String::from_utf8(output.stdout).map_err(|e| format!("Invalid UTF-8 output: {}", e))
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Compilation failed: {}", stderr))
    }
}

/// Compile liar source to lIR using liarliar (self-hosted compiler)
pub fn compile_to_lir_liarliar(source: &str) -> Result<String, String> {
    let liarliar_path = "/tmp/liarliar";

    if !Path::new(liarliar_path).exists() {
        return Err("liarliar binary not found at /tmp/liarliar".to_string());
    }

    // Pipe source to liarliar via stdin
    use std::io::Write;
    use std::process::Stdio;

    let mut child = Command::new(liarliar_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn liarliar: {}", e))?;

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(source.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for liarliar: {}", e))?;

    if output.status.success() {
        String::from_utf8(output.stdout).map_err(|e| format!("Invalid UTF-8 output: {}", e))
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("liarliar compilation failed: {}", stderr))
    }
}

/// Check if USE_LIARLIAR environment variable is set
pub fn use_liarliar() -> bool {
    std::env::var("USE_LIARLIAR").is_ok()
}

/// Normalize lIR output for comparison (remove extra whitespace, normalize newlines)
pub fn normalize_lir(lir: &str) -> String {
    lir.lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}
