Feature: Literal Compilation
  Verifies that liar literals compile to the correct lIR representation.

  Scenario: Integer literal
    Given the liar expression 42
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i64 42)
    And the output contains (ret (i64 42))

  Scenario: Negative integer literal
    Given the liar expression -17
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i64 -17)

  Scenario: Zero literal
    Given the liar expression 0
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i64 0)

  Scenario: Boolean true literal
    Given the liar expression true
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 1)

  Scenario: Boolean false literal
    Given the liar expression false
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 0)
