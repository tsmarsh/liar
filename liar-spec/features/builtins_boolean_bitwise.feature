Feature: Builtin Boolean and Bitwise Operations
  Verifies boolean and bitwise builtins compile to the expected lIR.

  Scenario Outline: Boolean builtins
    Given the liar expression (<op> true false)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op  | lir  |
      | and | (and |
      | or  | (or  |

  Scenario: Boolean not builtin
    Given the liar expression (not false)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (xor

  Scenario: Nil predicate builtin
    Given the liar expression (nil? nil)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (icmp eq
    And the output contains (ptr null)

  Scenario Outline: Boolean builtins arity
    Given the liar expression (<op> true)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op  |
      | and |
      | or  |

  Scenario: Not builtin arity
    Given the liar expression (not true false)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument

  Scenario Outline: Bitwise builtins
    Given the liar expression (<op> 5 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op                   | lir    |
      | bit-and              | (and   |
      | bit-or               | (or    |
      | bit-xor              | (xor   |
      | bit-shift-left       | (shl   |
      | shl                  | (shl   |
      | bit-shift-right      | (lshr  |
      | shr                  | (lshr  |
      | arithmetic-shift-right | (ashr |
      | ashr                 | (ashr  |

  Scenario: Bitwise not builtin
    Given the liar expression (bit-not 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (xor (i64 -1)

  Scenario: Popcount builtin
    Given the liar expression (popcount 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (ctpop

  Scenario Outline: Bitwise builtins arity
    Given the liar expression (<op> 1)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op              |
      | bit-and         |
      | bit-or          |
      | bit-xor         |
      | bit-shift-left  |
      | bit-shift-right |
      | shl             |
      | shr             |
      | arithmetic-shift-right |
      | ashr            |

  Scenario Outline: Unary bitwise builtins arity
    Given the liar expression (<op> 1 2)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument

    Examples:
      | op       |
      | bit-not  |
      | popcount |
