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

  # Full struct field access (requires function definitions)
  # These scenarios test the complete flow: define struct, allocate, access fields
  Scenario: Access first field of a two-field struct
    Given the struct definition (defstruct point (i64 i64))
    And the function definition (define (get-x i64) ((ptr p)) (block entry (let ((field-ptr (getelementptr %struct.point p (i32 0) (i32 0)))) (ret (load i64 field-ptr)))))
    When I call get-x with a point struct containing (42, 100)
    Then the result is (i64 42)

  Scenario: Access second field of a two-field struct
    Given the struct definition (defstruct point (i64 i64))
    And the function definition (define (get-y i64) ((ptr p)) (block entry (let ((field-ptr (getelementptr %struct.point p (i32 0) (i32 1)))) (ret (load i64 field-ptr)))))
    When I call get-y with a point struct containing (42, 100)
    Then the result is (i64 100)

  Scenario: Modify struct field through GEP
    Given the struct definition (defstruct counter (i64))
    And the function definition (define (set-value void) ((ptr p) (i64 val)) (block entry (let ((field-ptr (getelementptr %struct.counter p (i32 0) (i32 0)))) (store val field-ptr) (ret))))
    When I set counter value to 999
    Then the counter contains (i64 999)
