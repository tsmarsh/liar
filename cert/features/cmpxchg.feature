Feature: Compare-and-Exchange (CmpXchg) Operations

  lIR supports compare-and-exchange (cmpxchg) atomic operations that atomically
  compare a value and swap if equal. Returns a struct { old_value, success_flag }.

  Scenario: CmpXchg success - returns old value
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (extractvalue result 0))))))
    When I call test
    Then the result is (i64 10)

  Scenario: CmpXchg success - check memory was updated
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 20)

  Scenario: CmpXchg success - check success flag is true
    Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (extractvalue result 1))))))
    When I call test
    Then the result is (i1 1)

  Scenario: CmpXchg failure - returns old value
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 0))))))
    When I call test
    Then the result is (i64 10)

  Scenario: CmpXchg failure - memory unchanged
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 10)

  Scenario: CmpXchg failure - check success flag is false
    Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 1))))))
    When I call test
    Then the result is (i1 0)

  Scenario: CmpXchg with i32 type
    Given the expression (define (test i32) () (block entry (let ((p (alloca i32))) (store (i32 42) p) (let ((result (cmpxchg seq_cst p (i32 42) (i32 100)))) (ret (extractvalue result 0))))))
    When I call test
    Then the result is (i32 42)

  Scenario: CmpXchg with monotonic ordering
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 5) p) (let ((result (cmpxchg monotonic p (i64 5) (i64 10)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 10)

  Scenario: CmpXchg with acquire-release ordering
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 100) p) (let ((result (cmpxchg acq_rel p (i64 100) (i64 200)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 200)

  Scenario: CmpXchg multiple times - first succeeds
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 1) p) (let ((r1 (cmpxchg seq_cst p (i64 1) (i64 2)))) (let ((r2 (cmpxchg seq_cst p (i64 2) (i64 3)))) (ret (load i64 p)))))))
    When I call test
    Then the result is (i64 3)

  Scenario: CmpXchg zero to non-zero
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 0) p) (let ((result (cmpxchg seq_cst p (i64 0) (i64 999)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 999)

  Scenario: CmpXchg non-zero to zero
    Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 999) p) (let ((result (cmpxchg seq_cst p (i64 999) (i64 0)))) (ret (load i64 p))))))
    When I call test
    Then the result is (i64 0)

  Scenario: CmpXchg with pointer type - success
    Given the expression (define (test i1) () (block entry (let ((pp (alloca ptr)) (target (alloca i64)) (other (alloca i64))) (store target pp) (let ((result (cmpxchg seq_cst pp target other))) (ret (extractvalue result 1))))))
    When I call test
    Then the result is (i1 1)

  Scenario: CmpXchg with pointer type - failure
    Given the expression (define (test i1) () (block entry (let ((pp (alloca ptr)) (target (alloca i64)) (other (alloca i64)) (wrong (alloca i64))) (store target pp) (let ((result (cmpxchg seq_cst pp wrong other))) (ret (extractvalue result 1))))))
    When I call test
    Then the result is (i1 0)

