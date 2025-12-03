Feature: GetElementPtr with Struct Types

  Support for getelementptr with named struct types.
  This is essential for accessing struct fields through pointers.

  # Parser support for %struct.name syntax
  Scenario: Parse GEP with struct type
    Given the expression (getelementptr %struct.point (ptr null) (i32 0) (i32 0))
    Then parsing succeeds

  Scenario: Parse GEP with struct type and inbounds
    Given the expression (getelementptr inbounds %struct.adder_env (ptr null) (i32 0) (i32 0))
    Then parsing succeeds

  # Full struct field access - self-contained tests
  Scenario: Access first field of a two-field struct
    Given the expression (defstruct point (i64 i64))
    And the expression (define (test-get-x i64) () (block entry (let ((p (alloca i64 (i32 2)))) (store (i64 42) (getelementptr %struct.point p (i32 0) (i32 0))) (store (i64 100) (getelementptr %struct.point p (i32 0) (i32 1))) (ret (load i64 (getelementptr %struct.point p (i32 0) (i32 0)))))))
    When I call test-get-x
    Then the result is (i64 42)

  Scenario: Access second field of a two-field struct
    Given the expression (defstruct point (i64 i64))
    And the expression (define (test-get-y i64) () (block entry (let ((p (alloca i64 (i32 2)))) (store (i64 42) (getelementptr %struct.point p (i32 0) (i32 0))) (store (i64 100) (getelementptr %struct.point p (i32 0) (i32 1))) (ret (load i64 (getelementptr %struct.point p (i32 0) (i32 1)))))))
    When I call test-get-y
    Then the result is (i64 100)

  Scenario: Modify struct field through GEP
    Given the expression (defstruct counter (i64))
    And the expression (define (test-set-value i64) () (block entry (let ((p (alloca i64))) (store (i64 999) (getelementptr %struct.counter p (i32 0) (i32 0))) (ret (load i64 (getelementptr %struct.counter p (i32 0) (i32 0)))))))
    When I call test-set-value
    Then the result is (i64 999)
