# liarliar: Name Resolution

Create `lib/liarliar/resolve.liar` - resolve names to bindings with scope tracking.

## Overview

Track lexical scopes using hash maps. Detect undefined variables and shadowing.

## Scope Structure

```clojure
(defstruct Scope (bindings: ptr parent: ptr))

(defun make-scope (parent)
  (share (Scope (str-map) parent)))

(defun lookup (scope name)
  (let ((found (sm-get (. scope bindings) name)))
    (if found
        found
        (if (nil? (. scope parent))
            nil
            (lookup (. scope parent) name)))))

(defun bind (scope name info)
  (sm-assoc (. scope bindings) name info))
```

## Main Resolver

```clojure
(defun resolve (ctx scope form)
  (cond
    ((symbol? form) (resolve-symbol ctx scope form))
    ((not (cons? form)) form)
    ((let? form) (resolve-let ctx scope form))
    ((fn? form) (resolve-fn ctx scope form))
    ((defun? form) (resolve-defun ctx scope form))
    (true (map-form (fn (x) (resolve ctx scope x)) form))))

(defun resolve-symbol (ctx scope sym)
  (let ((name (symbol-name sym)))
    (if (or (builtin? name) (lookup scope name))
        sym
        (error! ctx "undefined symbol" sym))))
```

## Binding Forms

```clojure
(defun resolve-let (ctx scope form)
  ;; (let ((x 1) (y 2)) body)
  (let ((bindings (cadr form))
        (body (caddr form))
        (new-scope (make-scope scope)))
    (do
      (resolve-bindings ctx scope new-scope bindings)
      (list 'let bindings (resolve ctx new-scope body)))))

(defun resolve-fn (ctx scope form)
  ;; (fn (x y) body)
  (let ((params (cadr form))
        (body (caddr form))
        (fn-scope (make-scope scope)))
    (do
      (bind-params fn-scope params)
      (list 'fn params (resolve ctx fn-scope body)))))
```

## Builtins List

Pre-register: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `not`, `and`, `or`, `nil?`, `cons?`, `car`, `cdr`, `cons`, `list`, `if`, `do`, `let`, `fn`, `quote`, `print`, `println`

## Functions to Implement

- `resolve` - Main dispatcher
- `resolve-symbol` - Check symbol is defined
- `resolve-let` - Handle let bindings
- `resolve-fn` - Handle lambda params
- `resolve-defun` - Handle function definitions
- `builtin?` - Check if symbol is builtin

## Dependencies

- `lib/liarliar/value.liar` - Tagged value predicates
- `lib/liarliar/symbols.liar` - Symbol names
- `liar.hashmap` - Scope bindings

## Test Cases

- Undefined variable -> error
- Shadowing works correctly
- Let introduces new bindings
- Lambda params are in scope for body
