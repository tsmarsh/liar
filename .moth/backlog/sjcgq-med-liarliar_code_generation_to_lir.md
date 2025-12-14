# liarliar: Code Generation to lIR

Create `lib/liarliar/codegen.liar` - emit lIR S-expressions as strings.

## Overview

Walk the AST (tagged Cons cells) and emit corresponding lIR code using StringBuilder.

## Expression Codegen

```clojure
(defun codegen-expr (ctx sb form)
  (cond
    ((int? form)
     (sb-append sb "(i64 ")
     (sb-append-int sb (int-value form))
     (sb-append sb ")"))

    ((symbol? form)
     (sb-append sb "%")
     (sb-append sb (mangle (symbol-name form))))

    ((add? form)
     (sb-append sb "(add ")
     (codegen-expr ctx sb (cadr form))
     (sb-append sb " ")
     (codegen-expr ctx sb (caddr form))
     (sb-append sb ")"))

    ((if? form)
     (codegen-if ctx sb form))

    ((let? form)
     (codegen-let ctx sb form))

    ((call? form)
     (codegen-call ctx sb form))

    ;; ... more forms
    ))
```

## Function Codegen

```clojure
(defun codegen-defun (ctx sb form)
  (sb-append sb "(define ")
  (sb-append sb (return-type form))
  (sb-append sb " @")
  (sb-append sb (defun-name form))
  (sb-append sb " (")
  (codegen-params ctx sb form)
  (sb-append sb ") (block entry ")
  (codegen-expr ctx sb (defun-body form))
  (sb-append sb "))\n"))
```

## Builtin Codegen

Map liar builtins to lIR:
- `+` -> `add`
- `-` -> `sub`
- `*` -> `mul`
- `/` -> `sdiv`
- `=` -> `icmp eq`
- `<` -> `icmp slt`
- etc.

## Functions to Implement

- `codegen-expr` - Main expression dispatcher
- `codegen-defun` - Function definitions
- `codegen-if` - Conditional with blocks
- `codegen-let` - Let bindings
- `codegen-call` - Function calls
- `codegen-builtin` - Builtin operations
- `codegen-closure` - Closure literals
- `mangle` - Name mangling for lIR

## Dependencies

- `lib/liarliar/strings.liar` - StringBuilder
- `lib/liarliar/value.liar` - Tagged value predicates

## Milestones

1. `(defun main () (+ 1 2))` - Integers and arithmetic
2. `(defun square (x) (* x x))` - Function params
3. `(if (< x 0) (- 0 x) x)` - Conditionals
4. Closures and higher-order functions
