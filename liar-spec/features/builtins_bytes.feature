Feature: Builtin Byte and Pointer Operations
  Verifies byte-level memory and pointer arithmetic builtins compile to the expected lIR.

  Scenario: Store byte builtin
    Given the liar expression (store-byte (ptr+ (heap-array 1) 0) 65)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (store
    And the output contains (trunc i8

  Scenario: Load byte builtin
    Given the liar expression (load-byte (ptr+ (heap-array 1) 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (load i8
    And the output contains (zext i64

  Scenario: Pointer arithmetic builtin
    Given the liar expression (ptr+ (heap-array 1) 0)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (getelementptr inbounds i8

  Scenario Outline: Byte builtins arity
    Given the liar expression (<op> (heap-array 1))
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op        |
      | store-byte |
      | ptr+      |

  Scenario: Load-byte arity
    Given the liar expression (load-byte 1 2)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument
