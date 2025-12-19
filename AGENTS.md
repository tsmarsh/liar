# Repository Guidelines

## Project Structure & Module Organization
- `liar/` is the high-level Lisp compiler; core passes live in `liar/src/` (parser, macro expansion, inference, ownership, codegen).
- `lir-core/`, `lir-codegen/`, `lir-repl/`, `lir-cli/`, `lir-lair/` are the lIR stack and tools.
- `liar-repl/` and `liar-nrepl/` provide REPL and IDE integration.
- `liar-runtime/` contains the async runtime used by liar programs.
- Specs and docs live in `doc/` and ADRs in `doc/adr/`.
- BDD feature tests are under `cert/features/` (lIR) and `liar-cert/features/` (liar). The standard library lives in `lib/`.

## Build, Test, and Development Commands
- `cargo build --release` builds the full toolchain binaries into `target/release/`.
- `cargo test` runs the workspace unit tests and integration tests.
- `cargo test --test cert` runs the lIR BDD suite (286 scenarios).
- `cargo test -p liar-cert --test cert` runs the liar BDD suite (117 scenarios).
- `./target/release/liar-repl` starts the REPL after a release build.

## Coding Style & Naming Conventions
- Rust edition is 2021. Use standard `rustfmt` defaults (no repo-specific config).
- Keep files small and focused: prefer splitting if a file exceeds ~500 lines or a function exceeds ~50 lines.
- Avoid hidden globals and thread-locals in compiler passes; pass context explicitly.
- Keep lIR crates generic: no liar-specific terminology in `lir-*` crates.
- Lisp library files in `lib/` use namespaced filenames like `liar.core.liar`.

## Testing Guidelines
- Unit tests live near code (`#[cfg(test)]` modules).
- Feature specs use Cucumber via `cert/` and `liar-cert/`; add new scenarios to the relevant `features/*.feature` files.
- Tests should distinguish “not implemented” (non-zero exit) vs “incorrect” behavior in BDD runs.

## Commit & Pull Request Guidelines
- Commit messages commonly use conventional prefixes: `feat:`, `fix:`, `chore:`. Some commits are prefixed with a moth issue ID like `[abc12] feat: ...`.
- If using moth, link work by prefixing commits with the issue ID (e.g., `[abc12] Fix parser edge case`).
- PRs should include: a short summary, test commands run, and any relevant feature files or docs updated.

## Agent-Specific Notes (Moth)
- Issues live in `.moth/` and can be managed via `moth ls`, `moth start {id}`, `moth done`.
- If the task is not tied to a current issue, create one with `moth new "Title" --no-edit`.
