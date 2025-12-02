# Cucumber JIT Backend

## Goal
Create a proper LirBackend that uses the JIT REPL to evaluate expressions,
replacing the StubBackend that marks all tests as pending.

## Tasks
1. Create a CLI tool that takes an expression and outputs the result
2. Implement JitBackend in cert/src/lib.rs
3. Update cert/tests/features.rs to use JitBackend
4. Run cucumber tests and verify passing/failing status

## Notes
- The backend should call `lir-cli` or similar with the expression
- Exit code 0 on success, non-zero on error
- stdout should contain the result in canonical form
- stderr should contain error messages
