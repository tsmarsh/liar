## Summary
Add `let` bindings to name intermediate SSA values. This is **critical** — most 
feature tests use `let` but it's not implemented.

## Syntax
```lisp
(let ((name1 expr1)
      (name2 expr2))
  body...)
```

## Examples
```lisp
; Simple binding
(let ((x (i32 42)))
  (ret x))

; Multiple bindings  
(let ((a (alloca i32))
      (b (i32 5)))
  (store b a)
  (ret (load i32 a)))

; Nested let
(let ((x (i32 1)))
  (let ((y (add x (i32 2))))
    (ret y)))

; With phi (from control_flow.feature)
(block loop
  (let ((i (phi i32 (entry (i32 0)) (loop next-i)))
        (acc (phi i32 (entry (i32 0)) (loop next-acc))))
    (let ((next-i (add i (i32 1)))
          (next-acc (add acc i)))
      (br (icmp sle i n) loop done))))
```

## Implementation

### AST addition
```rust
// In ast.rs, add to Expr enum:
Let {
    bindings: Vec<(String, Box<Expr>)>,  // (name, value)
    body: Vec<Expr>,                      // body expressions
}
```

### Parser addition
```rust
// In parse_form match:
"let" => self.parse_let(),

// New method:
fn parse_let(&mut self) -> Result<Expr, ParseError> {
    // Expect opening paren for bindings list
    self.expect(Token::LParen)?;
    
    let mut bindings = Vec::new();
    while !matches!(self.lexer.peek()?, Some(Token::RParen)) {
        self.expect(Token::LParen)?;
        let name = self.expect_ident()?;
        let value = self.parse_expr()?;
        self.expect(Token::RParen)?;
        bindings.push((name, Box::new(value)));
    }
    self.expect(Token::RParen)?;  // close bindings list
    
    // Parse body expressions until outer RParen
    let mut body = Vec::new();
    while !matches!(self.lexer.peek()?, Some(Token::RParen)) {
        body.push(self.parse_expr()?);
    }
    
    Ok(Expr::Let { bindings, body })
}
```

### Codegen addition
```rust
// In compile_expr_recursive or compile_expr_inner:
Expr::Let { bindings, body } => {
    let mut new_locals = locals.clone();
    
    // Evaluate each binding and add to locals
    for (name, expr) in bindings {
        let value = self.compile_expr_recursive(expr, &new_locals)?;
        new_locals.insert(name.clone(), value);
    }
    
    // Evaluate body expressions, return last
    let mut result = None;
    for expr in body {
        result = self.compile_expr_with_locals(expr, &new_locals)?;
    }
    
    result.ok_or_else(|| CodeGenError::CodeGen("empty let body".to_string()))
}
```

## Acceptance Criteria
- [ ] Add `Let` variant to `Expr` enum
- [ ] Add `parse_let()` to parser
- [ ] Add let handling to codegen with scope extension
- [ ] Feature tests in memory.feature pass
- [ ] Feature tests in control_flow.feature pass

## Notes
- This unblocks the majority of existing feature tests
- Let bindings extend the locals map, not replace it
- Body returns the value of the last expression
