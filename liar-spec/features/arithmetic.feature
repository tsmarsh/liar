Feature: Arithmetic Compilation
  Verifies that liar arithmetic operations compile to the correct lIR instructions.

  Scenario: Addition
    Given the liar expression (+ 1 2)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (add

  Scenario: Subtraction
    Given the liar expression (- 5 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (sub

  Scenario: Multiplication
    Given the liar expression (* 4 5)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (mul

  Scenario: Division
    Given the liar expression (/ 10 2)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (sdiv

  Scenario: Remainder
    Given the liar expression (rem 7 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (srem

  Scenario: Nested arithmetic
    Given the liar expression (+ (* 2 3) (- 10 5))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (add
    And the output contains (mul
    And the output contains (sub
