## Summary
50 scenarios are skipped because there's no step definition for calling compiled
functions. The functions compile successfully (Given steps pass), but we can't
invoke them.

## Missing steps
```gherkin
When I call {function_name}
When I call {function_name} with {args...}
Then the result is {expected}
```

## Current architecture
The test harness shells out to `lir` CLI per expression. Each `Given the expression`
is independent. For function calls, we need to:
1. Accumulate multiple definitions in one JIT context
2. Call a function by name with arguments
3. Get the result back

## Implementation approach

### Option A: Extend CLI with multi-expression mode
Add a mode to `lir` CLI that accepts multiple expressions and a call:
```bash
lir --call factorial '(define (factorial i64) ...) (i64 5)'
# Outputs: (i64 120)
```

### Option B: Use JIT directly in test harness (recommended)
Modify `cert.rs` to use JIT directly instead of shelling out.

```rust
use lir_codegen::jit::Jit;
use lir_core::parser::Parser;

#[derive(Debug, Default, World)]
pub struct LirWorld {
    jit: Option<Jit>,
    items: Vec<String>,  // Accumulated definitions
    last_result: Option<String>,
}

#[given(regex = r"^the expression (.+)$")]
async fn given_expression(world: &mut LirWorld, expr: String) {
    world.items.push(expr);
}

#[when(regex = r"^I call (\w+)$")]
async fn when_call_no_args(world: &mut LirWorld, name: String) {
    call_function(world, &name, &[]);
}

#[when(regex = r"^I call (\w+) with (.+)$")]
async fn when_call_with_args(world: &mut LirWorld, name: String, args: String) {
    let arg_values = parse_arg_list(&args);
    call_function(world, &name, &arg_values);
}

fn call_function(world: &mut LirWorld, name: &str, args: &[Value]) {
    // Build combined source from all accumulated items
    let source = world.items.join("\n");
    
    // Parse all items
    let mut parser = Parser::new(&source);
    let items = parser.parse_items()?;
    
    // Create JIT, add all functions
    let jit = Jit::new();
    for item in items {
        jit.add_item(item)?;
    }
    
    // Call the target function
    let result = jit.call(name, args)?;
    world.last_result = Some(format_result(&result));
}

#[then(regex = r"^the result is (.+)$")]
async fn then_result_is(world: &mut LirWorld, expected: String) {
    let actual = world.last_result.as_ref().expect("no result");
    assert_eq!(actual, &expected);
}
```

### JIT API additions needed
The `Jit` struct needs:
```rust
impl Jit {
    pub fn add_item(&mut self, item: Item) -> Result<(), Error>;
    pub fn call(&self, name: &str, args: &[Value]) -> Result<Value, Error>;
}
```

This is similar to what the unit tests already do, just exposed as a clean API.

## Files to modify
1. `lir-codegen/src/jit.rs` - add `add_item()` and `call()` public API
2. `lir-cli/tests/cert.rs` - use JIT directly, add When steps
3. `lir-cli/Cargo.toml` - add dev-dependency on lir-codegen

## Edge cases
- Recursive calls (factorial calls itself) - JIT must resolve
- Multiple functions before call
- Void functions: `Then the function completes successfully`
- Pointer results: `Then the result is a pointer`
- External declarations (declare) before call

## Acceptance criteria
- [ ] JIT has public `call(name, args)` API
- [ ] `When I call X` step definition works
- [ ] `When I call X with (i32 5)` step definition works
- [ ] control_flow.feature: 0 skipped
- [ ] functions.feature: 0 skipped  
- [ ] integration_milestone.feature: 0 skipped
- [ ] Final: 201 passed, 0 skipped
