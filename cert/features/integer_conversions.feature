Feature: Integer Conversions (trunc, zext, sext)
  lIR supports LLVM's integer conversion instructions.
  trunc narrows, zext/sext widen with zero/sign extension.

  Scenario: trunc - Truncate to smaller integer
    Given the expression (trunc i8 (i32 256))
    Then the result is (i8 0)

    Given the expression (trunc i8 (i32 257))
    Then the result is (i8 1)

    Given the expression (trunc i8 (i32 -1))
    Then the result is (i8 -1)

    Given the expression (trunc i16 (i32 65536))
    Then the result is (i16 0)

    Given the expression (trunc i32 (i64 4294967296))
    Then the result is (i32 0)

    Given the expression (trunc i1 (i8 2))
    Then the result is (i1 0)

    Given the expression (trunc i1 (i8 3))
    Then the result is (i1 1)

  Scenario: zext - Zero extend to larger integer
    Given the expression (zext i32 (i8 255))
    Then the result is (i32 255)

    Given the expression (zext i16 (i8 255))
    Then the result is (i16 255)

    Given the expression (zext i64 (i32 4294967295))
    Then the result is (i64 4294967295)

    Given the expression (zext i32 (i8 -1))
    Then the result is (i32 255)

    Given the expression (zext i8 (i1 1))
    Then the result is (i8 1)

    Given the expression (zext i32 (i1 1))
    Then the result is (i32 1)

  Scenario: sext - Sign extend to larger integer
    Given the expression (sext i32 (i8 127))
    Then the result is (i32 127)

    Given the expression (sext i32 (i8 -1))
    Then the result is (i32 -1)

    Given the expression (sext i32 (i8 -128))
    Then the result is (i32 -128)

    Given the expression (sext i64 (i32 -1))
    Then the result is (i64 -1)

    Given the expression (sext i16 (i8 -1))
    Then the result is (i16 -1)

    Given the expression (sext i8 (i1 1))
    Then the result is (i8 -1)

    Given the expression (sext i32 (i1 1))
    Then the result is (i32 -1)

  Scenario: zext vs sext comparison
    Given the expression (zext i16 (i8 128))
    Then the result is (i16 128)

    Given the expression (sext i16 (i8 128))
    Then the result is (i16 -128)

    Given the expression (zext i32 (i16 65535))
    Then the result is (i32 65535)

    Given the expression (sext i32 (i16 65535))
    Then the result is (i32 -1)

  Scenario: Multiple conversion widths
    Given the expression (zext i64 (i8 42))
    Then the result is (i64 42)

    Given the expression (sext i64 (i8 -42))
    Then the result is (i64 -42)

    Given the expression (trunc i8 (i64 1234567890))
    Then the result is (i8 -46)
