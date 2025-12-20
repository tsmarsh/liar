# Add Shadowing Lint (Clippy-like) for liarliar/liar

## Problem

Variable shadowing in `let` bindings can cause subtle bugs. In the segfault fix (commit 729d829), `ctx` was being shadowed in `codegen-list`, causing updates to be lost:

```lisp
(let ((name (symbol-name head))        ;; unused binding
      (base-name (symbol-base-name (cg-syms ctx) head)))
  ...)
```

The unused `name` binding was harmless, but patterns like:

```lisp
(let ((ctx (do-something ctx)))  ;; shadows outer ctx
  (let ((ctx (do-more ctx)))     ;; shadows again - easy to lose track
    ...))
```

Can lead to bugs where you think you're updating the outer `ctx` but you're not.

## Solution

Extend `liar-lint` (in `liar/src/lint.rs`) to detect shadowing:

1. Add `LintKind::Shadowing` variant
2. Walk expressions recursively, tracking variable scopes
3. Warn when a `let` binding shadows an outer variable

## Implementation

```rust
fn lint_expr(expr: &Expr, scopes: &mut Vec<HashSet<String>>, warnings: &mut Vec<LintWarning>) {
    match expr {
        Expr::Let { bindings, body, .. } => {
            scopes.push(HashSet::new());
            for binding in bindings {
                let name = &binding.name;
                // Check all outer scopes for shadowing
                for outer in scopes.iter().rev().skip(1) {
                    if outer.contains(name) {
                        warnings.push(LintWarning::new(
                            LintKind::Shadowing,
                            binding.span,
                            format!("'{}' shadows outer binding", name),
                        ));
                        break;
                    }
                }
                scopes.last_mut().unwrap().insert(name.clone());
            }
            lint_expr(body, scopes, warnings);
            scopes.pop();
        }
        // Handle other expression types...
    }
}
```

## Scope

- Function parameters create initial scope
- Each `let` creates a new scope level
- `fn` (lambda) parameters create a scope
- Warn on any shadowing (can add allow-list later if too noisy)

## Files to modify

- `liar/src/lint.rs` - add shadowing check
- Need to walk `Expr` tree (currently only walks `Item` level)
