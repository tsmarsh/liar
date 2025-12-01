Feature: Bitwise Operations (and, or, xor, shl, lshr, ashr)
  lIR supports LLVM's bitwise operations on integers.
  These also serve as boolean logic when used with i1.

  Scenario: and - Bitwise AND
    Given the expression (and (i32 0b1100) (i32 0b1010))
    Then the result is (i32 0b1000)

    Given the expression (and (i8 255) (i8 15))
    Then the result is (i8 15)

    Given the expression (and (i32 -1) (i32 255))
    Then the result is (i32 255)

  Scenario: or - Bitwise OR
    Given the expression (or (i32 0b1100) (i32 0b1010))
    Then the result is (i32 0b1110)

    Given the expression (or (i8 240) (i8 15))
    Then the result is (i8 255)

    Given the expression (or (i32 0) (i32 42))
    Then the result is (i32 42)

  Scenario: xor - Bitwise XOR
    Given the expression (xor (i32 0b1100) (i32 0b1010))
    Then the result is (i32 0b0110)

    Given the expression (xor (i8 255) (i8 255))
    Then the result is (i8 0)

    Given the expression (xor (i32 -1) (i32 0))
    Then the result is (i32 -1)

  Scenario: shl - Shift left
    Given the expression (shl (i32 1) (i32 4))
    Then the result is (i32 16)

    Given the expression (shl (i8 1) (i8 7))
    Then the result is (i8 -128)

    Given the expression (shl (i32 -1) (i32 8))
    Then the result is (i32 -256)

  Scenario: lshr - Logical shift right
    Given the expression (lshr (i32 16) (i32 2))
    Then the result is (i32 4)

    Given the expression (lshr (i8 -128) (i8 1))
    Then the result is (i8 64)

    Given the expression (lshr (i32 -1) (i32 24))
    Then the result is (i32 255)

  Scenario: ashr - Arithmetic shift right
    Given the expression (ashr (i32 16) (i32 2))
    Then the result is (i32 4)

    Given the expression (ashr (i8 -128) (i8 1))
    Then the result is (i8 -64)

    Given the expression (ashr (i32 -1) (i32 24))
    Then the result is (i32 -1)

  Scenario: Boolean logic with i1
    Given the expression (and (i1 1) (i1 1))
    Then the result is (i1 1)

    Given the expression (and (i1 1) (i1 0))
    Then the result is (i1 0)

    Given the expression (or (i1 0) (i1 0))
    Then the result is (i1 0)

    Given the expression (or (i1 1) (i1 0))
    Then the result is (i1 1)

    Given the expression (xor (i1 1) (i1 1))
    Then the result is (i1 0)

    Given the expression (xor (i1 1) (i1 0))
    Then the result is (i1 1)

  Scenario: Shift by zero
    Given the expression (shl (i32 42) (i32 0))
    Then the result is (i32 42)

    Given the expression (lshr (i32 42) (i32 0))
    Then the result is (i32 42)

    Given the expression (ashr (i32 42) (i32 0))
    Then the result is (i32 42)
