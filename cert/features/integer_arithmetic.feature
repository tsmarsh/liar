Feature: Integer Arithmetic (add, sub, mul, sdiv, udiv, srem, urem)
  lIR supports LLVM's integer arithmetic operations.
  Operations require matching types—no automatic promotion.

  Scenario: add - Integer addition
    Given the expression (add (i32 5) (i32 3))
    Then the result is (i32 8)

    Given the expression (add (i8 100) (i8 50))
    Then the result is (i8 -106)

    Given the expression (add (i64 1000000000000) (i64 2000000000000))
    Then the result is (i64 3000000000000)

  Scenario: sub - Integer subtraction
    Given the expression (sub (i32 10) (i32 3))
    Then the result is (i32 7)

    Given the expression (sub (i32 5) (i32 10))
    Then the result is (i32 -5)

    Given the expression (sub (i8 0) (i8 1))
    Then the result is (i8 -1)

  Scenario: mul - Integer multiplication
    Given the expression (mul (i32 6) (i32 7))
    Then the result is (i32 42)

    Given the expression (mul (i8 16) (i8 16))
    Then the result is (i8 0)

    Given the expression (mul (i32 -5) (i32 3))
    Then the result is (i32 -15)

  Scenario: sdiv - Signed integer division
    Given the expression (sdiv (i32 10) (i32 3))
    Then the result is (i32 3)

    Given the expression (sdiv (i32 -10) (i32 3))
    Then the result is (i32 -3)

    Given the expression (sdiv (i32 10) (i32 -3))
    Then the result is (i32 -3)

    Given the expression (sdiv (i32 -10) (i32 -3))
    Then the result is (i32 3)

  Scenario: udiv - Unsigned integer division
    Given the expression (udiv (i32 10) (i32 3))
    Then the result is (i32 3)

    Given the expression (udiv (i8 255) (i8 2))
    Then the result is (i8 127)

    Given the expression (udiv (i32 4294967295) (i32 2))
    Then the result is (i32 2147483647)

  Scenario: srem - Signed integer remainder
    Given the expression (srem (i32 10) (i32 3))
    Then the result is (i32 1)

    Given the expression (srem (i32 -10) (i32 3))
    Then the result is (i32 -1)

    Given the expression (srem (i32 10) (i32 -3))
    Then the result is (i32 1)

    Given the expression (srem (i32 -10) (i32 -3))
    Then the result is (i32 -1)

  Scenario: urem - Unsigned integer remainder
    Given the expression (urem (i32 10) (i32 3))
    Then the result is (i32 1)

    Given the expression (urem (i8 255) (i8 7))
    Then the result is (i8 3)

  Scenario: Operations across integer widths
    Given the expression (add (i8 1) (i8 2))
    Then the result is (i8 3)

    Given the expression (add (i16 1000) (i16 2000))
    Then the result is (i16 3000)

    Given the expression (add (i32 100000) (i32 200000))
    Then the result is (i32 300000)

    Given the expression (add (i64 10000000000) (i64 20000000000))
    Then the result is (i64 30000000000)

  Scenario: Overflow wraps around
    Given the expression (add (i8 127) (i8 1))
    Then the result is (i8 -128)

    Given the expression (sub (i8 -128) (i8 1))
    Then the result is (i8 127)

    Given the expression (mul (i32 2147483647) (i32 2))
    Then the result is (i32 -2)
