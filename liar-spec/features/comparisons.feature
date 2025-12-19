Feature: Comparison Compilation
  Verifies that liar comparison operations compile to the correct lIR instructions.

  Scenario: Equality comparison
    Given the liar code (defun test () (if (= 1 1) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp eq

  Scenario: Less than comparison
    Given the liar code (defun test () (if (< 1 2) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp slt

  Scenario: Greater than comparison
    Given the liar code (defun test () (if (> 2 1) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp sgt

  Scenario: Less than or equal comparison
    Given the liar code (defun test () (if (<= 1 1) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp sle

  Scenario: Greater than or equal comparison
    Given the liar code (defun test () (if (>= 2 1) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp sge
