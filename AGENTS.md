# Repository Guidelines

## Project Structure & Module Organization
- `liar/` is the high-level Lisp compiler; core passes live in `liar/src/` (parser, macro expansion, inference, ownership, codegen).
- `liarliar/` hosts the self-hosted compiler written in liar; `liar-spec/` and `liar-cert/` are BDD suites.
- `lir-core/`, `lir-codegen/`, `lir-repl/`, `lir-cli/`, `lir-lair/` are the lIR stack and tools.
- `liar-repl/` and `liar-nrepl/` provide REPL and IDE integration.
- `liar-runtime/` contains the async runtime used by liar programs.
- Specs and docs live in `doc/` and ADRs in `doc/adr/`. The standard library lives in `lib/`.

## Build, Test, and Development Commands
- `make` builds the release binaries into `target/release/` (`liarc`, `lair`, `liarliar`).
- `make test` runs `liar-spec` plus workspace tests (`cargo test`).
- `make liar-spec` runs the liar -> lIR spec suite and drops a `target/liar-spec.ok` marker.
- `make lint` runs `liar-lint` over `lib/*.liar` and `liarliar/*.liar`.
- `./target/release/liar-repl` starts the REPL after a release build.

## Coding Style & Naming Conventions
- Rust edition is 2021. Use standard `rustfmt` defaults (no repo-specific config).
- Keep files small and focused: prefer splitting if a file exceeds ~500 lines or a function exceeds ~50 lines.
- Avoid hidden globals and thread-locals in compiler passes; pass context explicitly.
- Keep lIR crates generic: no liar-specific terminology in `lir-*` crates.
- Lisp library files in `lib/` use namespaced filenames like `liar.prelude.liar`; run `make lint` before changes land.

## Testing Guidelines
- Unit tests live near code (`#[cfg(test)]` modules).
- Feature specs use Cucumber via `cert/`, `liar-cert/`, and `liar-spec/`; add scenarios in `features/*.feature`.
- Use `USE_LIARLIAR=1 LIARLIAR=target/release/liarliar cargo test -p liar-spec --test spec` to validate the self-hosted compiler.
- Tests should distinguish “not implemented” (non-zero exit) vs “incorrect” behavior in BDD runs.

## Commit & Pull Request Guidelines
- Commit messages commonly use conventional prefixes: `feat:`, `fix:`, `chore:`, plus workflow tags like `start:`/`done:`. Many commits are prefixed with a moth issue ID like `[abc12] fix: ...`.
- If using moth, link work by prefixing commits with the issue ID (e.g., `[abc12] Fix parser edge case`).
- PRs should include: a short summary, test commands run, and any relevant feature files or docs updated.

## Agent-Specific Notes (Moth)
- Issues live in `.moth/` and can be managed via `moth ls`, `moth start {id}`, `moth done`.
- If the task is not tied to a current issue, create one with `moth new "Title" --no-edit`.
