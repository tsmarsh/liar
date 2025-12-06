Feature: Struct Instantiation and Field Access

  Scenario: Create struct and access first field
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 10 20))) (. p x)))
    When I evaluate (test)
    Then the result is 10

  Scenario: Create struct and access second field
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 10 20))) (. p y)))
    When I evaluate (test)
    Then the result is 20

  Scenario: Struct with single field
    Given the definition (defstruct Wrapper (value: i64))
    Given the definition (defun test () (let ((w (Wrapper 42))) (. w value)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Struct field in arithmetic
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 3 4))) (+ (. p x) (. p y))))
    When I evaluate (test)
    Then the result is 7
