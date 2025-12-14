# liarliar: Closure Conversion

Create `lib/liarliar/closures.liar` - analyze captures and lift lambdas.

## Overview

1. Find free variables in lambda bodies
2. Determine capture mode (move/borrow/clone)
3. Generate environment structs
4. Lift lambdas to top-level functions

## Free Variable Detection

```clojure
(defun find-free-vars (form bound)
  (cond
    ((symbol? form)
     (if (member form bound) nil (list form)))
    ((not (cons? form)) nil)
    ((fn? form)
     (find-free-vars (fn-body form)
                     (append (fn-params form) bound)))
    (true (flatmap (fn (x) (find-free-vars x bound)) form))))
```

## Capture Mode

```clojure
;; Modes:
;; :move - Transfer ownership into closure
;; :borrow - Borrow reference (closure cannot escape)
;; :clone - Clone value into closure

(defun determine-mode (var body)
  ;; Analyze how var is used in body
  ;; If mutated: borrow-mut
  ;; If only read: borrow (if doesn't escape) or clone
  ;; Default: move
  :move)
```

## Lambda Conversion

```clojure
(defun convert-lambda (ctx form)
  (let ((params (fn-params form))
        (body (fn-body form))
        (free (find-free-vars body params))
        (fn-name (gensym ctx "__lambda"))
        (env-name (str fn-name "_env")))
    (do
      ;; Emit environment struct
      (emit-struct ctx env-name free)
      ;; Emit lifted function (takes env as first param)
      (emit-lifted-fn ctx fn-name env-name params body free)
      ;; Return closure literal
      (make-closure-lit fn-name env-name free))))

(defun make-closure-lit (fn-name env-name captures)
  (if (nil? captures)
      (list 'closure-lit fn-name nil)
      (list 'closure-lit fn-name
            (cons 'share (cons (make-env-alloc env-name captures) nil)))))
```

## Escape Analysis

```clojure
;; Determine if closure escapes its defining scope
;; If returned from function -> escapes (heap allocate)
;; If only called locally -> doesn't escape (can stack allocate)

(defun escapes? (ctx form)
  (or (returned? ctx form)
      (stored-in-struct? ctx form)
      (passed-to-escaping-fn? ctx form)))
```

## Functions to Implement

- `find-free-vars` - Detect captured variables
- `determine-mode` - Choose capture mode
- `convert-lambda` - Main conversion
- `emit-struct` - Generate env struct definition
- `emit-lifted-fn` - Generate top-level function
- `make-closure-lit` - Create closure literal AST
- `escapes?` - Escape analysis
- `gensym` - Generate unique names

## Dependencies

- `lib/liarliar/value.liar` - Tagged value predicates
- `lib/liarliar/symbols.liar` - Name generation

## Test Cases

- `(fn (x) (+ x 1))` - No captures
- `(let ((y 1)) (fn (x) (+ x y)))` - Captures y
- Returning closure -> heap allocated
- Local closure -> stack allocated
