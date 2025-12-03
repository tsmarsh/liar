//! Linker invocation

use crate::args::Args;
use anyhow::{bail, Result};
use std::path::Path;
use std::process::Command;

/// Link object file(s) into an executable or shared library
pub fn link(args: &Args, object_path: &Path) -> Result<()> {
    let output = args
        .output
        .as_ref()
        .expect("output path required for linking");

    let mut cmd = Command::new("cc");

    cmd.arg(object_path);
    cmd.arg("-o").arg(output);

    if args.shared {
        cmd.arg("-shared");
    }

    if args.static_link {
        cmd.arg("-static");
    }

    if args.pic {
        cmd.arg("-fPIC");
    }

    if args.strip {
        cmd.arg("-s");
    }

    for lib in &args.libs {
        cmd.arg(format!("-l{}", lib));
    }

    for path in &args.lib_paths {
        cmd.arg(format!("-L{}", path.display()));
    }

    for arg in &args.linker_args {
        cmd.arg(format!("-Wl,{}", arg));
    }

    if args.verbose {
        eprintln!("Linking: {:?}", cmd);
    }

    let status = cmd.status()?;
    if !status.success() {
        bail!(
            "linker failed with exit code: {}",
            status.code().unwrap_or(-1)
        );
    }

    Ok(())
}
