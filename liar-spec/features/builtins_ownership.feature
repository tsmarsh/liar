Feature: Builtin Ownership and Memory Operations
  Verifies ownership and allocation builtins compile to the expected lIR.

  Scenario: Alloc builtin
    Given the liar expression (alloc)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (alloc own i64)

  Scenario: Alloc builtin arity
    Given the liar expression (alloc 1)
    When I compile to lIR
    Then compilation fails
    And the error contains requires no arguments

  Scenario Outline: Unary ownership builtins
    Given the liar expression (<op> (alloc))
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op   | lir        |
      | drop | (drop      |
      | move | (move      |

  Scenario: Share builtin
    Given the liar code:
      """
      (defstruct Point (x: i64 y: i64))
      (defun test () (share (Point 1 2)))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (heap-struct Point

  Scenario Outline: Reference counting builtins
    Given the liar expression (<op> 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op      | lir       |
      | rc-new  | (rc-alloc |
      | rc-clone | (rc-clone |
      | rc-drop | (rc-drop |
      | clone   | (rc-clone |

  Scenario Outline: Unary ownership builtins require one argument
    Given the liar expression (<op> 1 2)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument

    Examples:
      | op      |
      | drop    |
      | move    |
      | rc-new  |
      | rc-clone |
      | rc-drop |
      | clone   |
      | share   |

  Scenario: Share requires a struct constructor
    Given the liar expression (share 1)
    When I compile to lIR
    Then compilation fails
    And the error contains share requires a struct constructor argument
