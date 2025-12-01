# lIR (liar)

An S-expression assembler for LLVM IR. Not a Lisp—just LLVM IR in parens.

## Philosophy

**Spec-first BDD development.** Feature files define the language specification. Tests have three states:
- **Green**: Implemented correctly
- **Yellow/Pending**: Not implemented yet (runtime exits non-zero)
- **Red**: Implemented wrong (runtime gives wrong answer)

The distinction between "not yet implemented" and "broken" is critical. A non-zero exit code from the runtime means WIP, not failure.

## Core Design

This is a 1:1 mapping to LLVM IR. No sugar, no promotion, no Lisp semantics. S-expressions are just the syntax.

```lisp
(fadd (double 5.0) (double 6.0))
```

Maps directly to:
```llvm
%0 = fadd double 5.0, 6.0
```

### Types

Use LLVM's type names exactly:

- **Integers:** `i1`, `i8`, `i16`, `i32`, `i64`
- **Floats:** `float`, `double`
- **Vectors:** `<4 x i32>`, `<2 x double>`, etc.

No `bool`—use `i1`. No `f32`/`f64`—use `float`/`double`.

```lisp
(i1 1)        ; true
(i1 0)        ; false
(i32 42)
(double 3.14)
```

### No Type Promotion

Types must match exactly. This is an error:
```lisp
(add (i8 1) (i32 2))  ; ERROR: type mismatch
```

You must explicitly convert:
```lisp
(add (sext i32 (i8 1)) (i32 2))  ; OK
```

### Operations

**Integer Arithmetic:**
- `add`, `sub`, `mul`
- `sdiv`, `udiv` (signed/unsigned division)
- `srem`, `urem` (signed/unsigned remainder)

**Float Arithmetic:**
- `fadd`, `fsub`, `fmul`, `fdiv`, `frem`

**Bitwise:**
- `and`, `or`, `xor`
- `shl` (shift left)
- `lshr` (logical shift right)
- `ashr` (arithmetic shift right)

**Integer Comparison (icmp):**
```lisp
(icmp eq (i32 5) (i32 5))   ; => (i1 1)
(icmp ne (i32 5) (i32 6))   ; => (i1 1)
(icmp slt (i32 -1) (i32 1)) ; => (i1 1) signed less than
(icmp ult (i32 -1) (i32 1)) ; => (i1 0) unsigned less than (-1 is MAX_UINT)
```
Predicates: `eq`, `ne`, `slt`, `sle`, `sgt`, `sge`, `ult`, `ule`, `ugt`, `uge`

**Float Comparison (fcmp):**
```lisp
(fcmp oeq (double 1.0) (double 1.0))  ; => (i1 1) ordered equal
(fcmp olt (double 1.0) (double 2.0))  ; => (i1 1) ordered less than
(fcmp uno (double nan) (double 1.0))  ; => (i1 1) unordered (either is NaN)
```
Predicates: `oeq`, `one`, `olt`, `ole`, `ogt`, `oge`, `ord`, `uno`, `ueq`, `une`, `ult`, `ule`, `ugt`, `uge`

**Conversions:**
- `trunc` — truncate to smaller int
- `zext` — zero extend to larger int
- `sext` — sign extend to larger int
- `fptrunc` — truncate to smaller float
- `fpext` — extend to larger float
- `fptoui` — float to unsigned int
- `fptosi` — float to signed int
- `uitofp` — unsigned int to float
- `sitofp` — signed int to float

**Select:**
```lisp
(select (icmp slt (i32 5) (i32 10)) (i32 1) (i32 2))  ; => (i32 1)
```

**Vectors:**
- `extractelement`
- `insertelement`
- `shufflevector`

## What This Is Not

- No `if`/`then`/`else` — use `select`, or eventually `br`/`phi`/blocks
- No implicit conversions
- No `bool` type
- No operator overloading (int `add` vs float `fadd`)
- No `and`/`or` on booleans — use bitwise `and`/`or` on `i1`

## Project Structure

```
lir/
├── CLAUDE.md
├── cert/
│   └── features
```

Note: The current feature files need rewriting to match this spec. They were written with Lisp sugar (type promotion, `bool`, combined `add` for int/float, invented comparison ops). Delete them and start fresh.


## Why

We're building a real Lisp on top of this. lIR is the foundation—an assembler layer. The language above will have the sugar, the promotion rules, the ergonomics. This layer is just LLVM IR with S-expression syntax.


# Moth Agent Guide

This guide helps LLM agents work effectively with moth, a git-based file issue tracker.

## Overview

Moth stores issues as markdown files in `.moth/` directories organized by status (ready, doing, done). Each issue has a unique ID, severity, and slug derived from the title.

## File Structure

```
.moth/
├── config.yml          # Project configuration
├── .current            # Current issue ID (when working on an issue)
├── ready/              # Issues ready to start
│   └── {id}-{severity}-{slug}.md
├── doing/              # Issues in progress
│   └── {id}-{severity}-{slug}.md
└── done/               # Completed issues
    └── {id}-{severity}-{slug}.md
```

Prioritized issues have a numeric prefix: `001-{id}-{severity}-{slug}.md`

## Workflow Commands

### Viewing Issues

```bash
# List all active issues (excludes done)
moth ls

# List issues in specific status
moth ls -t ready
moth ls -t doing

# List all issues including done
moth ls -a

# Filter by severity
moth ls -s high
moth ls -s crit

# Show current issue details
moth show

# Show specific issue
moth show {id}
```

### Working on Issues

```bash
# Start working on an issue (moves to doing, sets as current)
moth start {id}

# Mark issue as done
moth done {id}

# Mark current issue as done
moth done

# Move issue to any status
moth mv {id} {status}
```

### Creating Issues

```bash
# Create new issue (opens editor)
moth new "Fix login bug"

# Create with severity
moth new "Critical security fix" -s crit

# Create without opening editor
moth new "Quick fix" --no-edit

# Create and immediately start working
moth new "Urgent task" --start
```

### Issue Management

```bash
# Edit issue content
moth edit {id}

# Delete issue
moth rm {id}

# Change severity
moth severity {id} high
```

### Priority Management

```bash
# Set priority number
moth priority {id} 1

# Move to top priority
moth priority {id} top

# Move to bottom (removes priority)
moth priority {id} bottom

# Position relative to another issue
moth priority {id} above {other_id}
moth priority {id} below {other_id}

# Renumber priorities sequentially
moth compact
moth compact ready
```

## Severity Levels

From highest to lowest:
- `crit` - Critical, must fix immediately
- `high` - High priority
- `med` - Medium priority (default)
- `low` - Low priority

## Partial ID Matching

All commands accept partial IDs. If you have issue `abc12`, you can use:
- `moth show abc12` (full)
- `moth show abc1` (partial)
- `moth show a` (if unambiguous)

## Git Integration

### Commit Hook

Moth can auto-prefix commit messages with the current issue ID:

```bash
# Install the hook
moth hook install

# With existing hook
moth hook install --append

# Remove hook
moth hook uninstall
```

When active, commits are prefixed: `[abc12] Your commit message`

### Commit Message Format

When committing changes related to an issue, prefix with the issue ID:

```bash
git commit -m "[abc12] Fix authentication bypass"
```

This links commits to issues in the report.

## Generating Reports

```bash
# Full history as CSV
moth report

# From specific commit
moth report --since abc123

# Between commits
moth report --since abc123 --until def456
```

Output includes: commit info, story changes (created, moved, edited, deleted), and code commits referencing issues.

## Agent Best Practices

### Starting Work

1. Check current issues: `moth ls`
2. Find issue to work on or check current: `moth show`
3. Start working: `moth start {id}`

### During Development

1. Make changes and commit frequently
2. Prefix commits with issue ID: `[{id}] description`
3. Keep issue content updated if requirements change

### Completing Work

1. Ensure all changes committed
2. Mark issue done: `moth done`
3. The `.current` file is automatically cleared

### Creating New Issues

When user requests new work:
1. Create issue: `moth new "Title" -s {severity} --no-edit`
2. Optionally start immediately with `--start` flag
3. Update issue file with detailed requirements if needed

### Checking Status

```bash
# Quick status check
moth ls

# What am I working on?
moth show

# Full project state
moth ls -a
```

## Configuration Reference

`.moth/config.yml`:

```yaml
statuses:
  - name: ready
    dir: ready
    prioritized: true    # Enable priority ordering
  - name: doing
    dir: doing
  - name: done
    dir: done

default_severity: med    # Default for new issues
editor: vi               # Editor for moth edit
id_length: 5             # Length of generated IDs
no_edit: false           # Skip editor on moth new

priority:
  auto_compact: false    # Auto-renumber after priority changes
```

## Common Patterns

### Pick up next priority issue
```bash
moth ls -t ready
moth start {first-id}
```

### Quick bug fix
```bash
moth new "Fix typo in header" -s low --no-edit --start
# make fix
git commit -m "[{id}] Fix typo"
moth done
```

### Triage incoming work
```bash
moth new "Investigate performance issue" -s med --no-edit
moth priority {id} top
```

### Review what was done
```bash
moth ls -t done
moth report --since HEAD~10
```
