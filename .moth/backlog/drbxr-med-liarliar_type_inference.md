# liarliar: Type Inference

Create `lib/liarliar/infer.liar` - Hindley-Milner type inference with unification.

## Overview

Infer types for all expressions. Use type variables and unification to resolve constraints.

## Type Representation

```clojure
;; Types as tagged values/maps:
;; {:tag :int}
;; {:tag :ptr}
;; {:tag :var :id 42}         ;; type variable
;; {:tag :fn :params [t1 t2] :ret t3}  ;; function type
;; {:tag :struct :name "Point"}
```

## Inference Context

```clojure
(defstruct InferCtx
  (subs: ptr        ;; Substitutions: var-id -> type
   types: ptr       ;; Symbol -> type bindings
   next-var: i64))  ;; Counter for fresh type variables

(defun fresh-var (ctx)
  (let ((id (. ctx next-var)))
    ;; Return new ctx with incremented counter, and the type var
    {:ctx (update-next-var ctx) :type {:tag :var :id id}}))
```

## Main Inference

```clojure
(defun infer (ctx form)
  (cond
    ((int? form) {:type {:tag :int} :ctx ctx})
    ((string? form) {:type {:tag :ptr} :ctx ctx})
    ((symbol? form) (lookup-type ctx form))
    ((cons? form) (infer-call ctx form))
    (true {:type {:tag :ptr} :ctx ctx})))

(defun infer-call (ctx form)
  (let ((head (car form)))
    (cond
      ((builtin? head) (infer-builtin ctx head (cdr form)))
      ((if? form) (infer-if ctx form))
      ((let? form) (infer-let ctx form))
      ((fn? form) (infer-fn ctx form))
      (true (infer-app ctx form)))))
```

## Unification

```clojure
(defun unify (ctx t1 t2)
  (let ((t1 (apply-subs ctx t1))
        (t2 (apply-subs ctx t2)))
    (cond
      ((type-eq t1 t2) ctx)
      ((var? t1) (extend-subs ctx (var-id t1) t2))
      ((var? t2) (extend-subs ctx (var-id t2) t1))
      ((and (fn-type? t1) (fn-type? t2)) (unify-fn ctx t1 t2))
      (true (error "type mismatch")))))

(defun apply-subs (ctx ty)
  (if (var? ty)
      (let ((bound (lookup-sub ctx (var-id ty))))
        (if bound (apply-subs ctx bound) ty))
      ty))
```

## Functions to Implement

- `infer` - Main dispatcher
- `infer-call` - Infer function application
- `infer-builtin` - Type builtins (+, -, etc.)
- `infer-if` - Both branches must unify
- `infer-let` - Infer binding, extend env
- `infer-fn` - Create function type
- `unify` - Unification algorithm
- `apply-subs` - Apply substitutions
- `fresh-var` - Generate type variable

## Dependencies

- `lib/liarliar/value.liar` - Tagged value predicates
- `liar.hashmap` - Substitution map

## Test Cases

- `(+ 1 2)` -> int
- `(fn (x) (+ x 1))` -> fn(int)->int
- `(if true 1 2)` -> int
- Type error: `(+ 1 "hello")`
