#!/bin/bash

# Priority 1: Functions
moth new "lIR: function definitions (define)" -s crit --no-edit --stdin << 'EOF'
## Summary
Add function definition support to lIR.

## Syntax
```lisp
(define (name return-type) ((param-type param-name) ...) body...)
```

## Examples
```lisp
(define (add-one i32) ((i32 x))
  (ret (add x (i32 1))))

(define (get-42 i32) ()
  (ret (i32 42)))

(define (void-fn void) ()
  (ret))
```

## LLVM IR Output
```llvm
define i32 @add-one(i32 %x) {
entry:
  %0 = add i32 %x, 1
  ret i32 %0
}
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature`
- [ ] Parse function definitions
- [ ] Generate LLVM function
- [ ] Handle parameters
- [ ] Handle void return type
- [ ] Entry block created automatically

## Notes
- **Spec-first**: Write the feature file BEFORE implementing. Tests have three states: green (passing), yellow (not implemented), red (broken). See CLAUDE.md.
- **ADR-019**: All frontends target lIR. This is foundational for liar to emit.
- **ADR-020**: lair (the assembler) is written in Rust and emits LLVM IR via inkwell.
EOF

moth new "lIR: ret instruction" -s crit --no-edit --stdin << 'EOF'
## Summary
Add ret (return) instruction to lIR.

## Syntax
```lisp
(ret value)     ; return with value
(ret)           ; void return
```

## Examples
```lisp
(ret (i32 42))
(ret (add (i32 1) (i32 2)))
(ret)
```

## LLVM IR Output
```llvm
ret i32 42
ret i32 %0
ret void
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature` (same file as define)
- [ ] Parse ret with value
- [ ] Parse void ret
- [ ] Type check return value matches function return type
- [ ] Generates terminator instruction

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Ret is a terminator instruction - every basic block must end with one.
EOF

moth new "lIR: function calls (call)" -s crit --no-edit --stdin << 'EOF'
## Summary
Add function call support to lIR.

## Syntax
```lisp
(call @function-name arg1 arg2 ...)
```

## Examples
```lisp
(call @add-one (i32 41))
(call @printf (ptr @format) (i32 42))
(call @exit (i32 0))
```

## LLVM IR Output
```llvm
%0 = call i32 @add-one(i32 41)
call void @exit(i32 0)
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature`
- [ ] Parse call expressions
- [ ] Resolve function references
- [ ] Type check arguments
- [ ] Handle void vs non-void return

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-019**: Functions are essential - liar compiles to lIR function calls.
EOF

# Priority 2: Memory
moth new "lIR: pointer type (ptr)" -s crit --no-edit --stdin << 'EOF'
## Summary
Add opaque pointer type to lIR.

## Syntax
```lisp
ptr             ; opaque pointer type
(ptr null)      ; null pointer
```

## Notes
LLVM now uses opaque pointers (just `ptr`), not typed pointers (`i32*`).

## Acceptance Criteria
- [ ] Feature file in `cert/features/pointers.feature`
- [ ] Add ptr to type system
- [ ] Parse null pointer literals
- [ ] Pointer type in function signatures

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Opaque pointers are the only pointer type in modern LLVM (15+).
EOF

moth new "lIR: alloca instruction" -s crit --no-edit --stdin << 'EOF'
## Summary
Add stack allocation to lIR.

## Syntax
```lisp
(alloca type)           ; allocate one element
(alloca type count)     ; allocate array
```

## Examples
```lisp
(alloca i32)            ; allocate space for one i32
(alloca double)         ; allocate space for one double
(alloca i8 (i64 100))   ; allocate 100 bytes
```

## LLVM IR Output
```llvm
%0 = alloca i32
%1 = alloca double
%2 = alloca i8, i64 100
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse alloca with type
- [ ] Parse alloca with count
- [ ] Returns ptr type

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Alloca is for stack allocation. Essential for local variables.
EOF

moth new "lIR: load instruction" -s crit --no-edit --stdin << 'EOF'
## Summary
Add memory load to lIR.

## Syntax
```lisp
(load type ptr-value)
```

## Examples
```lisp
(load i32 %ptr)
(load double %dptr)
```

## LLVM IR Output
```llvm
%0 = load i32, ptr %ptr
%1 = load double, ptr %dptr
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse load with type and pointer
- [ ] Type check pointer argument
- [ ] Returns the loaded type

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

moth new "lIR: store instruction" -s crit --no-edit --stdin << 'EOF'
## Summary
Add memory store to lIR.

## Syntax
```lisp
(store value ptr)
```

## Examples
```lisp
(store (i32 42) %ptr)
(store %value %ptr)
```

## LLVM IR Output
```llvm
store i32 42, ptr %ptr
store i32 %value, ptr %ptr
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse store with value and pointer
- [ ] Type check value and pointer
- [ ] Generates store instruction (returns void)

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Store is a side effect, returns void.
EOF

moth new "lIR: getelementptr instruction" -s high --no-edit --stdin << 'EOF'
## Summary
Add pointer arithmetic to lIR.

## Syntax
```lisp
(getelementptr type ptr index...)
(getelementptr inbounds type ptr index...)
```

## Examples
```lisp
; Array indexing
(getelementptr i32 %arr (i64 5))

; Struct field access
(getelementptr %struct.point %p (i32 0) (i32 1))
```

## LLVM IR Output
```llvm
%0 = getelementptr i32, ptr %arr, i64 5
%1 = getelementptr inbounds %struct.point, ptr %p, i32 0, i32 1
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse GEP with indices
- [ ] Handle inbounds flag
- [ ] Type calculation for result

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- GEP is complex but essential for array/struct access.
EOF

# Priority 3: Control Flow
moth new "lIR: basic blocks and labels" -s crit --no-edit --stdin << 'EOF'
## Summary
Add basic block support to lIR.

## Syntax
```lisp
(define (name ret-type) (params...)
  (block entry
    ...)
  (block label2
    ...)
  (block label3
    ...))
```

## Examples
```lisp
(define (abs i32) ((i32 x))
  (block entry
    (br (icmp slt x (i32 0)) neg pos))
  (block neg
    (ret (sub (i32 0) x)))
  (block pos
    (ret x)))
```

## LLVM IR Output
```llvm
define i32 @abs(i32 %x) {
entry:
  %0 = icmp slt i32 %x, 0
  br i1 %0, label %neg, label %pos
neg:
  %1 = sub i32 0, %x
  ret i32 %1
pos:
  ret i32 %x
}
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse block definitions
- [ ] Named labels
- [ ] First block is entry point

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Basic blocks are the foundation of control flow in SSA form.
EOF

moth new "lIR: unconditional branch (br)" -s crit --no-edit --stdin << 'EOF'
## Summary
Add unconditional branch to lIR.

## Syntax
```lisp
(br label)
```

## Examples
```lisp
(br loop)
(br exit)
```

## LLVM IR Output
```llvm
br label %loop
br label %exit
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse unconditional br
- [ ] Resolve label references
- [ ] Terminator instruction

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

moth new "lIR: conditional branch (br cond)" -s crit --no-edit --stdin << 'EOF'
## Summary
Add conditional branch to lIR.

## Syntax
```lisp
(br condition true-label false-label)
```

## Examples
```lisp
(br (icmp eq x (i32 0)) is-zero not-zero)
(br %flag then else)
```

## LLVM IR Output
```llvm
br i1 %0, label %is-zero, label %not-zero
br i1 %flag, label %then, label %else
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse conditional br
- [ ] Condition must be i1
- [ ] Resolve both label references

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

moth new "lIR: phi instruction" -s crit --no-edit --stdin << 'EOF'
## Summary
Add phi nodes for SSA form.

## Syntax
```lisp
(phi type (value1 label1) (value2 label2) ...)
```

## Examples
```lisp
(phi i32 ((i32 0) entry) (%next loop))
(phi ptr ((%a then) (%b else)))
```

## LLVM IR Output
```llvm
%0 = phi i32 [ 0, %entry ], [ %next, %loop ]
%1 = phi ptr [ %a, %then ], [ %b, %else ]
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse phi with incoming values
- [ ] All values must have same type
- [ ] Labels must exist

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Phi nodes are essential for SSA - they select values based on which block we came from.
EOF

# Priority 4: Globals
moth new "lIR: global variables" -s high --no-edit --stdin << 'EOF'
## Summary
Add global variable definitions.

## Syntax
```lisp
(global name type initializer)
(global name type initializer :constant)
```

## Examples
```lisp
(global counter i32 (i32 0))
(global pi double (double 3.14159) :constant)
(global message ptr (string "Hello\n") :constant)
```

## LLVM IR Output
```llvm
@counter = global i32 0
@pi = constant double 3.14159
@message = constant [7 x i8] c"Hello\0A\00"
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse global definitions
- [ ] Handle constant flag
- [ ] Initialize with value

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

moth new "lIR: external declarations (declare)" -s high --no-edit --stdin << 'EOF'
## Summary
Add external function declarations.

## Syntax
```lisp
(declare name return-type (param-types...))
```

## Examples
```lisp
(declare printf i32 (ptr ...))
(declare malloc ptr (i64))
(declare exit void (i32))
```

## LLVM IR Output
```llvm
declare i32 @printf(ptr, ...)
declare ptr @malloc(i64)
declare void @exit(i32)
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse declare
- [ ] Handle varargs (...)
- [ ] Functions callable but not defined

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-013**: External declarations are how lIR calls C/system functions (unsafe boundary).
EOF

moth new "lIR: string constants" -s high --no-edit --stdin << 'EOF'
## Summary
Add string literal support.

## Syntax
```lisp
(string "content")
```

## Examples
```lisp
(global fmt ptr (string "Value: %d\n") :constant)
(call @printf (getelementptr i8 @fmt (i64 0)) (i32 42))
```

## LLVM IR Output
```llvm
@fmt = constant [11 x i8] c"Value: %d\0A\00"
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse string literals
- [ ] Handle escape sequences (\n, \t, \0, \\, \")
- [ ] Generate as constant array

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Strings are null-terminated [N x i8] arrays in LLVM.
EOF

# Priority 5: Aggregates
moth new "lIR: struct types" -s med --no-edit --stdin << 'EOF'
## Summary
Add struct type definitions.

## Syntax
```lisp
(defstruct name (field-type ...))
```

## Examples
```lisp
(defstruct point (double double))
(defstruct person (ptr i32 double))
```

## LLVM IR Output
```llvm
%struct.point = type { double, double }
%struct.person = type { ptr, i32, double }
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/aggregates.feature`
- [ ] Parse struct definitions
- [ ] Named struct types
- [ ] Use in other types

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

moth new "lIR: extractvalue and insertvalue" -s med --no-edit --stdin << 'EOF'
## Summary
Add aggregate value access.

## Syntax
```lisp
(extractvalue aggregate index...)
(insertvalue aggregate value index...)
```

## Examples
```lisp
(extractvalue %point 0)              ; get first field
(insertvalue %point (double 1.0) 0)  ; set first field
```

## LLVM IR Output
```llvm
%0 = extractvalue %struct.point %point, 0
%1 = insertvalue %struct.point %point, double 1.0, 0
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/aggregates.feature`
- [ ] Parse extractvalue
- [ ] Parse insertvalue
- [ ] Handle nested indices

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
EOF

# Priority 6: Variables
moth new "lIR: local variable references" -s crit --no-edit --stdin << 'EOF'
## Summary
Add local variable references (SSA values).

## Syntax
```lisp
%name       ; reference a named value
```

## Examples
```lisp
(define (double-it i32) ((i32 x))
  (ret (add %x %x)))

; With let bindings for intermediate values
(define (example i32) ()
  (let ((a (i32 5))
        (b (add %a (i32 3))))
    (ret %b)))
```

## Design Options
In true SSA, every assignment creates a new value. Options for binding:

1. **Implicit numbering**: results are %0, %1, %2... (raw LLVM style)
2. **Explicit let binding**: `(let ((name expr)) body)` (Lisp style)
3. **Named results**: `(= name expr)` (assignment style)

Recommend option 2 (let) for consistency with liar.

## Acceptance Criteria
- [ ] Feature file in `cert/features/variables.feature`
- [ ] Parse variable references (%name)
- [ ] Track SSA values
- [ ] Parameters accessible by name
- [ ] Let bindings (if chosen)

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-019**: Variable binding strategy affects how liar emits lIR.
EOF

echo "Done! Run 'moth ls' to see created issues."