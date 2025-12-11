Feature: Reference Counting

  lIR supports reference-counted pointers with automatic memory management.

  Scenario: RC allocate and read
    Given the expression (define (test-rc i64) () (block entry (let ((x (rc-alloc i64))) (store (i64 42) (rc-ptr x)) (let ((v (load i64 (rc-ptr x)))) (rc-drop x) (ret v)))))
    When I call test-rc
    Then the result is (i64 42)

  Scenario: RC clone maintains value
    Given the expression (define (test-rc-clone i64) () (block entry (let ((x (rc-alloc i64))) (store (i64 42) (rc-ptr x)) (let ((y (rc-clone x))) (rc-drop x) (let ((v (load i64 (rc-ptr y)))) (rc-drop y) (ret v))))))
    When I call test-rc-clone
    Then the result is (i64 42)

  Scenario: RC count after alloc
    Given the expression (define (test-rc-count-1 i64) () (block entry (let ((x (rc-alloc i64))) (let ((c (rc-count x))) (rc-drop x) (ret c)))))
    When I call test-rc-count-1
    Then the result is (i64 1)

  Scenario: RC count after clone
    Given the expression (define (test-rc-count-2 i64) () (block entry (let ((x (rc-alloc i64))) (let ((y (rc-clone x))) (let ((c (rc-count x))) (rc-drop y) (rc-drop x) (ret c))))))
    When I call test-rc-count-2
    Then the result is (i64 2)

  Scenario: RC with i32 type
    Given the expression (define (test-rc-i32 i32) () (block entry (let ((x (rc-alloc i32))) (store (i32 123) (rc-ptr x)) (let ((v (load i32 (rc-ptr x)))) (rc-drop x) (ret v)))))
    When I call test-rc-i32
    Then the result is (i32 123)

  Scenario: RC with i8 type
    Given the expression (define (test-rc-i8 i8) () (block entry (let ((x (rc-alloc i8))) (store (i8 42) (rc-ptr x)) (let ((v (load i8 (rc-ptr x)))) (rc-drop x) (ret v)))))
    When I call test-rc-i8
    Then the result is (i8 42)

  Scenario: RC multiple clones
    Given the expression (define (test-rc-multi-clone i64) () (block entry (let ((x (rc-alloc i64))) (store (i64 77) (rc-ptr x)) (let ((y (rc-clone x))) (let ((z (rc-clone x))) (let ((c (rc-count x))) (rc-drop z) (rc-drop y) (rc-drop x) (ret c)))))))
    When I call test-rc-multi-clone
    Then the result is (i64 3)
