Feature: Core Language Tests

  Scenario: Simple addition
    Given the definition (defun test () (+ 1 2))
    When I evaluate (test)
    Then the result is 3

  Scenario: Simple subtraction
    Given the definition (defun test () (- 10 3))
    When I evaluate (test)
    Then the result is 7

  Scenario: Simple multiplication
    Given the definition (defun test () (* 6 7))
    When I evaluate (test)
    Then the result is 42

  Scenario: Integer division
    Given the definition (defun test () (/ 20 4))
    When I evaluate (test)
    Then the result is 5

  Scenario: Remainder
    Given the definition (defun test () (rem 17 5))
    When I evaluate (test)
    Then the result is 2

  Scenario: Let binding
    Given the definition (defun test () (let ((x 10)) x))
    When I evaluate (test)
    Then the result is 10
