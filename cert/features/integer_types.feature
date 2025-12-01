Feature: Integer Types (i1, i8, i16, i32, i64)
  lIR supports LLVM's integer types with their exact semantics.
  No bool type—use i1. Types must match exactly in operations.

  Scenario: i1 literals (boolean equivalent)
    Given the expression (i1 0)
    Then the result is (i1 0)

    Given the expression (i1 1)
    Then the result is (i1 1)

  Scenario: i8 literals
    Given the expression (i8 0)
    Then the result is (i8 0)

    Given the expression (i8 127)
    Then the result is (i8 127)

    Given the expression (i8 -128)
    Then the result is (i8 -128)

    Given the expression (i8 255)
    Then the result is (i8 -1)

  Scenario: i16 literals
    Given the expression (i16 0)
    Then the result is (i16 0)

    Given the expression (i16 32767)
    Then the result is (i16 32767)

    Given the expression (i16 -32768)
    Then the result is (i16 -32768)

    Given the expression (i16 65535)
    Then the result is (i16 -1)

  Scenario: i32 literals
    Given the expression (i32 0)
    Then the result is (i32 0)

    Given the expression (i32 2147483647)
    Then the result is (i32 2147483647)

    Given the expression (i32 -2147483648)
    Then the result is (i32 -2147483648)

    Given the expression (i32 4294967295)
    Then the result is (i32 -1)

  Scenario: i64 literals
    Given the expression (i64 0)
    Then the result is (i64 0)

    Given the expression (i64 9223372036854775807)
    Then the result is (i64 9223372036854775807)

    Given the expression (i64 -9223372036854775808)
    Then the result is (i64 -9223372036854775808)

  Scenario: Negative values use two's complement
    Given the expression (i8 -1)
    Then the result is (i8 -1)

    Given the expression (i16 -1)
    Then the result is (i16 -1)

    Given the expression (i32 -1)
    Then the result is (i32 -1)

    Given the expression (i64 -1)
    Then the result is (i64 -1)
