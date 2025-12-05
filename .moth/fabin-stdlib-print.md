# stdlib: print

**Priority:** P4  
**Category:** stdlib/io  
**Dependencies:** lIR FFI/extern support  
**Order:** 390

## Description

**BLOCKED: Requires I/O primitives in lIR**

Print a value to stdout without newline. Requires FFI or builtin I/O primitive.

## Implementation

```lisp
;; BLOCKED: Needs lIR support for extern/FFI
;; Example implementation once available:
;; (extern puts (ptr) i32)
;; (defun print (x) (unsafe (puts (to-cstring x))))
```

## Tests

```lisp
(print 42)        ; prints: 42
(print "hello")  ; prints: hello
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
