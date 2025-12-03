Feature: Atomic Read-Modify-Write Operations

  lIR supports atomic read-modify-write operations that perform an atomic
  operation on a memory location and return the old value.

  Scenario: Atomic add - returns old value
    Given the expression (define (test-add i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (ret (atomicrmw add seq_cst p (i64 5))))))
    When I call test-add
    Then the result is (i64 10)

  Scenario: Atomic add - verify new value
    Given the expression (define (test-add-new i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw add seq_cst p (i64 5)) (ret (load i64 p)))))
    When I call test-add-new
    Then the result is (i64 15)

  Scenario: Atomic sub - returns old value
    Given the expression (define (test-sub i64) () (block entry (let ((p (alloca i64))) (store (i64 20) p) (ret (atomicrmw sub seq_cst p (i64 7))))))
    When I call test-sub
    Then the result is (i64 20)

  Scenario: Atomic sub - verify new value
    Given the expression (define (test-sub-new i64) () (block entry (let ((p (alloca i64))) (store (i64 20) p) (atomicrmw sub seq_cst p (i64 7)) (ret (load i64 p)))))
    When I call test-sub-new
    Then the result is (i64 13)

  Scenario: Atomic xchg (swap)
    Given the expression (define (test-xchg i64) () (block entry (let ((p (alloca i64))) (store (i64 42) p) (ret (atomicrmw xchg seq_cst p (i64 100))))))
    When I call test-xchg
    Then the result is (i64 42)

  Scenario: Atomic xchg - verify new value
    Given the expression (define (test-xchg-new i64) () (block entry (let ((p (alloca i64))) (store (i64 42) p) (atomicrmw xchg seq_cst p (i64 100)) (ret (load i64 p)))))
    When I call test-xchg-new
    Then the result is (i64 100)

  Scenario: Atomic and
    Given the expression (define (test-and i64) () (block entry (let ((p (alloca i64))) (store (i64 15) p) (atomicrmw and seq_cst p (i64 7)) (ret (load i64 p)))))
    When I call test-and
    Then the result is (i64 7)

  Scenario: Atomic or
    Given the expression (define (test-or i64) () (block entry (let ((p (alloca i64))) (store (i64 3) p) (atomicrmw or seq_cst p (i64 12)) (ret (load i64 p)))))
    When I call test-or
    Then the result is (i64 15)

  Scenario: Atomic xor
    Given the expression (define (test-xor i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw xor seq_cst p (i64 7)) (ret (load i64 p)))))
    When I call test-xor
    Then the result is (i64 13)

  Scenario: Atomic max (signed)
    Given the expression (define (test-max i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw max seq_cst p (i64 20)) (ret (load i64 p)))))
    When I call test-max
    Then the result is (i64 20)

  Scenario: Atomic min (signed)
    Given the expression (define (test-min i64) () (block entry (let ((p (alloca i64))) (store (i64 20) p) (atomicrmw min seq_cst p (i64 10)) (ret (load i64 p)))))
    When I call test-min
    Then the result is (i64 10)

  Scenario: Atomic umax (unsigned)
    Given the expression (define (test-umax i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw umax seq_cst p (i64 20)) (ret (load i64 p)))))
    When I call test-umax
    Then the result is (i64 20)

  Scenario: Atomic umin (unsigned)
    Given the expression (define (test-umin i64) () (block entry (let ((p (alloca i64))) (store (i64 20) p) (atomicrmw umin seq_cst p (i64 10)) (ret (load i64 p)))))
    When I call test-umin
    Then the result is (i64 10)

  Scenario: Atomic add with monotonic ordering
    Given the expression (define (test-mono i64) () (block entry (let ((p (alloca i64))) (store (i64 5) p) (atomicrmw add monotonic p (i64 3)) (ret (load i64 p)))))
    When I call test-mono
    Then the result is (i64 8)

  Scenario: Atomic add with acquire-release
    Given the expression (define (test-acqrel i64) () (block entry (let ((p (alloca i64))) (store (i64 100) p) (atomicrmw add acq_rel p (i64 1)) (ret (load i64 p)))))
    When I call test-acqrel
    Then the result is (i64 101)

  Scenario: Atomic operations with i32
    Given the expression (define (test-i32 i32) () (block entry (let ((p (alloca i32))) (store (i32 50) p) (atomicrmw add seq_cst p (i32 25)) (ret (load i32 p)))))
    When I call test-i32
    Then the result is (i32 75)
