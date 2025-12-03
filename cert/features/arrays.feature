Feature: Bounds-Checked Arrays

  lIR supports fixed-size arrays with bounds-checked access.
  Arrays are allocated on the stack and provide safe access operations.

  Syntax:
    (array-alloc type size)           ; Stack allocate array
    (array-get type size arr idx)     ; Bounds-checked read
    (array-set type size arr idx val) ; Bounds-checked write
    (array-len size)                  ; Get length (compile-time constant)
    (array-ptr arr)                   ; Get raw pointer (for FFI)

  Scenario: Array length
    Given the expression (define (test-len i64) () (block entry (ret (array-len 10))))
    When I call test-len
    Then the result is (i64 10)

  Scenario: Array allocation and access
    Given the expression (define (test-array i64) () (block entry (let ((arr (array-alloc i64 10))) (array-set i64 10 arr (i64 5) (i64 42)) (ret (array-get i64 10 arr (i64 5))))))
    When I call test-array
    Then the result is (i64 42)

  Scenario: Static bounds elimination with constant index 0
    Given the expression (define (static-access i64) () (block entry (let ((arr (array-alloc i64 10))) (array-set i64 10 arr (i64 0) (i64 99)) (ret (array-get i64 10 arr (i64 0))))))
    When I call static-access
    Then the result is (i64 99)

  Scenario: Array with multiple elements
    Given the expression (define (multi-elem i64) () (block entry (let ((arr (array-alloc i64 3)) (a (i64 0)) (b (i64 0)) (c (i64 0))) (array-set i64 3 arr (i64 0) (i64 10)) (array-set i64 3 arr (i64 1) (i64 20)) (array-set i64 3 arr (i64 2) (i64 30)) (let ((a (array-get i64 3 arr (i64 0))) (b (array-get i64 3 arr (i64 1))) (c (array-get i64 3 arr (i64 2)))) (ret (add a (add b c)))))))
    When I call multi-elem
    Then the result is (i64 60)

  Scenario: Array with i32 elements
    Given the expression (define (test-i32-array i32) () (block entry (let ((arr (array-alloc i32 5))) (array-set i32 5 arr (i64 2) (i32 123)) (ret (array-get i32 5 arr (i64 2))))))
    When I call test-i32-array
    Then the result is (i32 123)
