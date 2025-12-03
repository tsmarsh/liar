## Summary
Add native closure support to lIR, replacing the struct+function encoding.

## Syntax
```lisp
; Closure definition
(closure NAME ((CAPTURE-MODE TYPE NAME) ...)
  (fn RETURN-TYPE ((TYPE PARAM) ...)
    BODY))

; Closure type declaration
(closuretype NAME RETURN-TYPE (PARAM-TYPES...))

; Closure call
(closure-call CLOSURE ARGS...)
```

## Capture Modes
```lisp
own    ; Move into closure, closure owns value
ref    ; Borrow, closure cannot escape scope  
rc     ; Reference-counted, shared ownership
```

## AST Additions
```rust
pub enum Item {
    // ... existing ...
    Closure {
        name: String,
        captures: Vec<Capture>,
        params: Vec<Param>,
        return_type: ReturnType,
        body: Vec<Block>,
    },
    ClosureType {
        name: String,
        return_type: ReturnType,
        param_types: Vec<ParamType>,
    },
}

pub struct Capture {
    pub mode: CaptureMode,
    pub ty: ParamType,
    pub name: String,
}

pub enum CaptureMode {
    Own,
    Ref,
    Rc,
}

pub enum Instruction {
    // ... existing ...
    ClosureCall {
        closure: Box<Expr>,
        args: Vec<Expr>,
    },
}
```

## Codegen Strategy
Closures compile to:
1. Environment struct (captures)
2. Function taking (env_ptr, args...)
3. Closure object = { env_ptr, fn_ptr }

```rust
// closure add-n ((own i64 n)) (fn i64 ((i64 x)) ...)
// becomes:

// 1. Env struct
%closure_add_n_env = type { i64 }

// 2. Function
define i64 @closure_add_n_fn(ptr %env, i64 %x) {
    %n_ptr = getelementptr %closure_add_n_env, ptr %env, i32 0, i32 0
    %n = load i64, ptr %n_ptr
    %result = add i64 %n, %x
    ret i64 %result
}

// 3. Closure type
%closuretype_IntToInt = type { ptr, ptr }  ; { env, fn }
```

## Closure Creation
```rust
// At creation site:
let env = self.builder.build_alloca(env_type, "closure_env");
// Store captures into env
for (i, capture) in captures.iter().enumerate() {
    let ptr = self.builder.build_gep(env, &[zero, i.into()], "cap_ptr");
    let val = self.get_local(&capture.name)?;
    self.builder.build_store(val, ptr);
}
// Build closure struct
let closure = self.builder.build_alloca(closure_type, "closure");
let env_field = self.builder.build_gep(closure, &[zero, zero], "env_field");
let fn_field = self.builder.build_gep(closure, &[zero, one], "fn_field");
self.builder.build_store(env, env_field);
self.builder.build_store(fn_ptr, fn_field);
```

## Closure Call
```rust
Instruction::ClosureCall { closure, args } => {
    let closure_val = self.compile_expr(closure)?;
    let env = self.builder.build_extract_value(closure_val, 0, "env");
    let fn_ptr = self.builder.build_extract_value(closure_val, 1, "fn");
    let mut call_args = vec![env];
    call_args.extend(args.iter().map(|a| self.compile_expr(a)).collect::<Result<Vec<_>>>()?);
    self.builder.build_indirect_call(fn_type, fn_ptr, &call_args, "closure_result")
}
```

## Test Cases
```gherkin
Scenario: Simple closure with capture
  Given the expression (closure add-n ((own i64 n)) (fn i64 ((i64 x)) (block entry (ret (add n x)))))
  And the expression (define (test-closure i64) () (block entry (let ((f (make-closure add-n (i64 10)))) (ret (closure-call f (i64 32))))))
  When I call test-closure
  Then the result is (i64 42)

Scenario: Closure with multiple captures
  Given the expression (closure add-mul ((own i64 a) (own i64 b)) (fn i64 ((i64 x)) (block entry (ret (add (mul a x) b)))))
  And the expression (define (test i64) () (block entry (let ((f (make-closure add-mul (i64 2) (i64 3)))) (ret (closure-call f (i64 10))))))
  When I call test
  Then the result is (i64 23)

Scenario: Closure passed to function
  Given the expression (closuretype IntToInt i64 (i64))
  And the expression (define (apply i64) ((own IntToInt f) (i64 x)) (block entry (ret (closure-call f x))))
  # ... setup closure and call apply
```

## Acceptance Criteria
- [ ] Closure syntax parses
- [ ] Closuretype parses
- [ ] Env struct generated correctly
- [ ] Captures stored in env
- [ ] closure-call works
- [ ] own capture moves value
- [ ] ref capture borrows (verification in borrow checker moth)
- [ ] Feature file passes
