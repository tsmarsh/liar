Feature: Struct Compilation
  Verifies that liar structs compile to the correct lIR representation.

  Scenario: Struct definition
    Given the liar code:
      """
      (defstruct Point (x: i64 y: i64))
      (defun test () 0)
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (defstruct Point

  Scenario: Struct construction
    Given the liar code:
      """
      (defstruct Point (x: i64 y: i64))
      (defun test () (Point 1 2))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (defstruct Point
    And the output contains (alloca

  Scenario: Struct field access
    Given the liar code:
      """
      (defstruct Point (x: i64 y: i64))
      (defun test () (let ((p (Point 1 2))) (. p x)))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (load
    And the output contains (getelementptr
