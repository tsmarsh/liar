Feature: Control Flow Compilation
  Verifies that liar control flow constructs compile to the correct lIR.

  Scenario: If with literal condition true
    Given the liar code (defun test () (if true 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 1)
    And the output contains (phi

  Scenario: If with literal condition false
    Given the liar code (defun test () (if false 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 0)
    And the output contains (phi

  Scenario: If with comparison
    Given the liar code (defun test () (if (= 1 1) 42 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp eq
    And the output contains (br
    And the output contains (phi

  Scenario: Nested if expressions
    Given the liar code (defun test () (if true (if false 1 2) 3))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (phi

  Scenario: Let binding
    Given the liar code (defun test () (let ((x 42)) x))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i64 42)

  Scenario: Multiple let bindings
    Given the liar code (defun test () (let ((x 1) (y 2)) (+ x y)))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (add
