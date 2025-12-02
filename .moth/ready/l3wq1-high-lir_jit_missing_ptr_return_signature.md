## Summary
Functions returning `ptr` can't be called from tests because the JIT
only has signatures for i64 returns.

## Current signatures (likely)
```rust
// In JIT call code
type NoArgsI64 = unsafe extern "C" fn() -> i64;
type OneArgI64 = unsafe extern "C" fn(i64) -> i64;
type TwoArgsI64 = unsafe extern "C" fn(i64, i64) -> i64;
```

## Missing signatures
```rust
type NoArgsPtr = unsafe extern "C" fn() -> *mut u8;
type OneArgI64Ptr = unsafe extern "C" fn(i64) -> *mut u8;
// etc.
```

## Fix approach
Add ptr return signatures and dispatch based on function return type:

```rust
fn call_function(&self, name: &str, args: &[i64]) -> Result<Value> {
    let func = self.get_function(name)?;
    let ret_type = func.get_return_type();
    
    match (ret_type, args.len()) {
        (ReturnType::Ptr, 0) => {
            let f: NoArgsPtr = transmute(func_ptr);
            let ptr = unsafe { f() };
            Ok(Value::Ptr(ptr))
        }
        (ReturnType::Ptr, 1) => {
            let f: OneArgI64Ptr = transmute(func_ptr);
            let ptr = unsafe { f(args[0]) };
            Ok(Value::Ptr(ptr))
        }
        (ReturnType::Scalar(I64), 0) => {
            let f: NoArgsI64 = transmute(func_ptr);
            let val = unsafe { f() };
            Ok(Value::I64(val))
        }
        // ... etc
    }
}
```

## Test cases
- integration_milestone.feature: Functions returning ptr (make_adder, etc.)

## Acceptance criteria
- [ ] Functions returning ptr can be called from tests
- [ ] Pointer values displayed as `(ptr 0x...)` or `(ptr null)`
