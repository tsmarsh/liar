# Stdlib Deps Validation

## Summary

Write a test file that validates liar's stdlib works for liarliar's needs. This is a **blocking prerequisite** for all other stdlib rewrite moths.

## Why This Matters

Before rewriting liarliar to use stdlib, we need to prove the stdlib actually works. If HashMap is broken, we'd waste time debugging liarliar when the bug is in liar.

## Deliverable

Create `liarliar/test-stdlib-deps.liar`:

```lisp
(ns liarliar.test-stdlib-deps
  (:require [liar.seq :refer :all]
            [liar.hashmap :refer :all]
            [liar.core :refer :all]))

;; === Sequences (liar.seq) ===

(defun test-cons ()
  (let ((lst (cons 1 (cons 2 (cons 3 nil)))))
    (assert-eq (first lst) 1)
    (assert-eq (first (rest lst)) 2)
    (assert-eq (first (rest (rest lst))) 3)))

(defun test-seq-accessors ()
  (let ((lst (cons 1 (cons 2 (cons 3 (cons 4 nil))))))
    (assert-eq (second lst) 2)
    (assert-eq (third lst) 3)
    (assert-eq (fourth lst) 4)))

(defun test-nil-handling ()
  (assert-eq (first nil) nil)
  (assert-eq (rest nil) nil))

;; === Hash Maps (liar.hashmap) ===

(defun test-hashmap-int-keys ()
  (let ((m (-> (hashmap) (assoc 1 "one") (assoc 2 "two"))))
    (assert-eq (get m 1) "one")
    (assert-eq (get m 2) "two")
    (assert-eq (get m 3) nil)))

(defun test-hashmap-string-keys ()
  ;; CRITICAL: liarliar uses string keys for symbol tables
  (let ((m (-> (hashmap) (assoc "foo" 42) (assoc "bar" 99))))
    (assert-eq (get m "foo") 42)
    (assert-eq (get m "bar") 99)
    (assert-eq (get m "baz") nil)))

(defun test-hashmap-update ()
  (let ((m1 (-> (hashmap) (assoc "x" 1)))
        (m2 (assoc m1 "x" 2)))
    (assert-eq (get m1 "x") 1)  ;; original unchanged
    (assert-eq (get m2 "x") 2))) ;; new version updated

;; === Core Functions ===

(defun test-threading ()
  ;; Test -> macro expansion
  (assert-eq (-> 1 inc inc inc) 4))

;; === Main ===

(defun main () -> i64
  (do
    (println "Testing stdlib dependencies...")
    (test-cons)
    (println "  cons: OK")
    (test-seq-accessors)
    (println "  seq accessors: OK")
    (test-nil-handling)
    (println "  nil handling: OK")
    (test-hashmap-int-keys)
    (println "  hashmap int keys: OK")
    (test-hashmap-string-keys)
    (println "  hashmap string keys: OK")
    (test-hashmap-update)
    (println "  hashmap update: OK")
    (test-threading)
    (println "  threading: OK")
    (println "All stdlib deps validated!")
    0))
```

## Acceptance Criteria

- [ ] `test-stdlib-deps.liar` compiles with liarc
- [ ] `test-stdlib-deps.liar` runs and prints "All stdlib deps validated!"
- [ ] If any test fails, file a bug against the appropriate stdlib component

## What To Do If Tests Fail

1. Write minimal reproduction
2. Create a moth for the stdlib bug
3. Fix in liar (not a workaround in liarliar)
4. Re-run this test

## Blocks

All other stdll-* moths depend on this passing.
