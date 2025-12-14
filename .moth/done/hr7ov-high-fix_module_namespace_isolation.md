# Fix Module Namespace Isolation

## Problem

The module system doesn't properly isolate namespaces. Currently:

1. All modules are merged into a flat `Program` via `into_merged_program()`
2. Resolver treats all definitions as being in the same scope
3. Different modules defining the same name (e.g., `any` in both liar.core and liar.seq) causes "duplicate definition" errors
4. `:as` aliasing doesn't work - names still pollute the global scope

## Expected Behavior

- `(:require [foo :as f])` - Only `f/name` works, no unqualified imports
- `(:require [foo :refer [bar]])` - Only `bar` is imported unqualified
- `(:require [foo :refer :all])` - All names imported unqualified
- Same-named definitions in different modules should NOT conflict

## Solution

1. **Per-module scopes**: Keep each module's definitions in a separate scope indexed by module name
2. **Qualified name resolution**: `seq/cons` looks up `cons` in the `seq` module's scope
3. **Import tracking**: Track which names are imported into current scope via `:refer`
4. **Lookup order**: local → referred → refer-all modules → builtins

## Files to Modify

- `liar/src/loader.rs` - Don't flatten modules, keep them separate
- `liar/src/resolve.rs` - Per-module scopes, proper qualified lookup
- `liar/src/ast.rs` - May need to track module origin for items

## Test Cases

```clojure
;; Should work: qualified access
(ns test (:require [liar.seq :as seq]))
(seq/cons 1 nil)

;; Should work: selective import
(ns test (:require [liar.seq :refer [cons]]))
(cons 1 nil)

;; Should fail: unqualified without import
(ns test (:require [liar.seq :as seq]))
(cons 1 nil)  ;; Error: undefined

;; Should work: same name in different modules
(ns test
  (:require [liar.core :refer :all]
            [liar.seq :as seq]))
;; Both modules can define 'any' without conflict
```
