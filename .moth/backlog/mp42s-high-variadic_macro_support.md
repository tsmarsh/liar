# Variadic Macro Support

Add rest parameter support for macros: `(defmacro name (x ... rest) body)`.

**Priority:** HIGH (required for threading macros ->, ->>)

## Context

Threading macros need to accept any number of forms:
```lisp
(-> x (f a) (g b) (h c))  ;; arbitrary number of forms
```

Currently macros have fixed arity. We need a rest parameter to collect remaining arguments.

## Design

### Syntax
```lisp
(defmacro -> (x ... forms)
  ;; x is first arg
  ;; forms is a list of remaining args
  ...)
```

The `...` before a parameter name marks it as the rest parameter. It collects all remaining arguments into a list.

### Alternative Syntax Options

1. Clojure-style: `(defmacro -> [x & forms] ...)`
2. Scheme-style: `(defmacro -> (x . forms) ...)`
3. Explicit keyword: `(defmacro -> (x :rest forms) ...)`
4. Ellipsis prefix: `(defmacro -> (x ... forms) ...)` (proposed)

Option 4 is consistent with extern varargs syntax and visually distinct.

## Implementation

### Parser Changes

In `parse_defmacro`, detect `...` before final parameter:

```rust
fn parse_defmacro(&mut self) -> Result<Defmacro> {
    let name = self.parse_symbol()?;
    self.expect(TokenKind::LParen)?;

    let mut params = Vec::new();
    let mut rest_param = None;

    while !self.check(TokenKind::RParen) {
        if self.check(TokenKind::Ellipsis) {
            self.advance(); // consume ...
            rest_param = Some(self.parse_symbol()?);
            break; // rest must be last
        }
        params.push(self.parse_symbol()?);
    }

    self.expect(TokenKind::RParen)?;
    let body = self.parse_expr()?;

    Ok(Defmacro { name, params, rest_param, body })
}
```

### AST Changes

```rust
pub struct Defmacro {
    pub name: String,
    pub params: Vec<String>,
    pub rest_param: Option<String>,  // NEW
    pub body: Expr,
}
```

### Macro Expansion Changes

In `expand_macro_call`, bind rest args to a list:

```rust
fn expand_macro_call(&mut self, macro_def: &MacroDef, args: &[Expr]) -> Result<Expr> {
    let mut env = Env::new();

    // Bind positional params
    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
        env.bind(param, arg.clone());
    }

    // Bind rest param to remaining args as a list
    if let Some(rest_name) = &macro_def.rest_param {
        let rest_args: Vec<_> = args[macro_def.params.len()..].to_vec();
        let rest_list = self.vec_to_cons_list(rest_args);
        env.bind(rest_name, rest_list);
    }

    self.eval_in_env(&macro_def.body, &env)
}
```

### Quasiquote Splicing

Rest params work naturally with splicing:
```lisp
(defmacro my-list (... items)
  `(list ,@items))

(my-list 1 2 3)  ;; => (list 1 2 3)
```

## Example: Threading Macros

```lisp
(defmacro -> (x ... forms)
  (if (nil? forms)
      x
      (let ((form (first forms))
            (rest-forms (rest forms)))
        (if (cons? form)
            `(-> (,(first form) ,x ,@(rest form)) ,@rest-forms)
            `(-> (,form ,x) ,@rest-forms)))))

(defmacro ->> (x ... forms)
  (if (nil? forms)
      x
      (let ((form (first forms))
            (rest-forms (rest forms)))
        (if (cons? form)
            `(->> (,@form ,x) ,@rest-forms)
            `(->> (,form ,x) ,@rest-forms)))))
```

## Test Cases

```lisp
;; Rest collects remaining args
(defmacro first-and-rest (x ... rest)
  `(list2 ,x (quote ,rest)))

(first-and-rest 1 2 3 4)  ;; => (list2 1 '(2 3 4))

;; Empty rest
(first-and-rest 1)  ;; => (list2 1 '())

;; Threading
(-> 5 inc inc)           ;; => 7
(-> (list3 1 2 3) (conj 4) (conj 5))
(->> (range 0 5) (map inc) (filter even?))
```

## Ordering

Required by: f7k2m (stdlib data transformation - threading macros)
Depends on: n5nax (instance? - needed for cons? in macro body)
