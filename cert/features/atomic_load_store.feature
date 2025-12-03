Feature: Atomic Load and Store

  lIR supports atomic memory operations with configurable memory ordering.
  These are low-level primitives for implementing lock-free data structures.

  Scenario: Atomic load and store with seq_cst
    Given the expression (define (test-atomic i64) () (block entry (let ((p (alloca i64))) (atomic-store seq_cst (i64 42) p) (ret (atomic-load seq_cst i64 p)))))
    When I call test-atomic
    Then the result is (i64 42)

  Scenario: Atomic load and store with acquire/release
    Given the expression (define (test-acquire-release i64) () (block entry (let ((p (alloca i64))) (atomic-store release (i64 100) p) (ret (atomic-load acquire i64 p)))))
    When I call test-acquire-release
    Then the result is (i64 100)

  Scenario: Atomic load and store with monotonic
    Given the expression (define (test-monotonic i64) () (block entry (let ((p (alloca i64))) (atomic-store monotonic (i64 77) p) (ret (atomic-load monotonic i64 p)))))
    When I call test-monotonic
    Then the result is (i64 77)

  Scenario: Atomic store followed by regular load
    Given the expression (define (test-mixed i64) () (block entry (let ((p (alloca i64))) (atomic-store seq_cst (i64 55) p) (ret (load i64 p)))))
    When I call test-mixed
    Then the result is (i64 55)

  Scenario: Regular store followed by atomic load
    Given the expression (define (test-mixed2 i64) () (block entry (let ((p (alloca i64))) (store (i64 33) p) (ret (atomic-load seq_cst i64 p)))))
    When I call test-mixed2
    Then the result is (i64 33)

  Scenario: Atomic operations with i32
    Given the expression (define (test-i32 i32) () (block entry (let ((p (alloca i32))) (atomic-store seq_cst (i32 12345) p) (ret (atomic-load seq_cst i32 p)))))
    When I call test-i32
    Then the result is (i32 12345)

  Scenario: Multiple atomic stores
    Given the expression (define (test-multi i64) () (block entry (let ((p (alloca i64))) (atomic-store seq_cst (i64 1) p) (atomic-store seq_cst (i64 2) p) (atomic-store seq_cst (i64 3) p) (ret (atomic-load seq_cst i64 p)))))
    When I call test-multi
    Then the result is (i64 3)
