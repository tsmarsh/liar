Feature: Ownership Types

  lIR supports ownership pointer types: own, ref, refmut.
  These compile to raw pointers at LLVM level; safety is enforced by verifier.

  Syntax:
    own T       ; Owned pointer - dropped when out of scope
    ref T       ; Shared borrow - read-only, lifetime-bound
    refmut T    ; Mutable borrow - exclusive, lifetime-bound

  Operations:
    (alloc own T)       ; Allocate owned pointer
    (borrow ref x)      ; Create shared borrow
    (borrow refmut x)   ; Create mutable borrow
    (drop x)            ; Explicit drop
    (move x)            ; Explicit move

  Scenario: Alloc and drop owned value
    Given the expression (define (test i64) () (block entry (let ((x (alloc own i64))) (store (i64 42) x) (let ((v (load i64 x))) (drop x) (ret v)))))
    When I call test
    Then the result is (i64 42)

  Scenario: Own parameter type
    Given the expression (define (take i64) ((own i64 x)) (block entry (let ((v (load i64 x))) (drop x) (ret v))))
    And the expression (define (test i64) () (block entry (let ((p (alloc own i64))) (store (i64 99) p) (ret (call @take (move p))))))
    When I call test
    Then the result is (i64 99)

  Scenario: Ref parameter type (shared borrow)
    Given the expression (define (peek i64) ((ref i64 x)) (block entry (ret (load i64 x))))
    And the expression (define (test i64) () (block entry (let ((p (alloc own i64))) (store (i64 77) p) (let ((v (call @peek (borrow ref p)))) (drop p) (ret v)))))
    When I call test
    Then the result is (i64 77)

  Scenario: Refmut parameter type (mutable borrow)
    Given the expression (define (poke void) ((refmut i64 x)) (block entry (store (i64 123) x) (ret)))
    And the expression (define (test i64) () (block entry (let ((p (alloc own i64))) (store (i64 0) p) (call @poke (borrow refmut p)) (let ((v (load i64 p))) (drop p) (ret v)))))
    When I call test
    Then the result is (i64 123)

