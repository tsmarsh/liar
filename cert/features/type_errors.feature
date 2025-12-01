Feature: Type Mismatch Errors
  lIR requires exact type matching. No implicit conversions.
  Type errors should be caught at compile time.

  Scenario: Mismatched integer operand types
    Given the expression (add (i8 1) (i32 2))
    Then it should error with "type mismatch"

    Given the expression (sub (i16 100) (i64 50))
    Then it should error with "type mismatch"

    Given the expression (mul (i32 5) (i8 3))
    Then it should error with "type mismatch"

  Scenario: Mismatched float operand types
    Given the expression (fadd (float 1.0) (double 2.0))
    Then it should error with "type mismatch"

    Given the expression (fmul (double 3.0) (float 4.0))
    Then it should error with "type mismatch"

  Scenario: Integer ops on float types
    Given the expression (add (double 1.0) (double 2.0))
    Then it should error with "integer operation on float"

    Given the expression (sub (float 5.0) (float 3.0))
    Then it should error with "integer operation on float"

    Given the expression (sdiv (double 10.0) (double 2.0))
    Then it should error with "integer operation on float"

  Scenario: Float ops on integer types
    Given the expression (fadd (i32 1) (i32 2))
    Then it should error with "float operation on integer"

    Given the expression (fmul (i64 5) (i64 3))
    Then it should error with "float operation on integer"

  Scenario: Bitwise ops on float types
    Given the expression (and (double 1.0) (double 2.0))
    Then it should error with "bitwise operation on float"

    Given the expression (shl (float 1.0) (float 2.0))
    Then it should error with "bitwise operation on float"

  Scenario: icmp on float types
    Given the expression (icmp eq (double 1.0) (double 1.0))
    Then it should error with "icmp requires integer"

  Scenario: fcmp on integer types
    Given the expression (fcmp oeq (i32 1) (i32 1))
    Then it should error with "fcmp requires float"

  Scenario: Mismatched comparison operands
    Given the expression (icmp eq (i32 5) (i64 5))
    Then it should error with "type mismatch"

    Given the expression (fcmp olt (float 1.0) (double 1.0))
    Then it should error with "type mismatch"

  Scenario: Select with mismatched value types
    Given the expression (select (i1 1) (i32 10) (i64 20))
    Then it should error with "type mismatch"

    Given the expression (select (i1 1) (float 1.0) (double 2.0))
    Then it should error with "type mismatch"

  Scenario: Select with non-i1 condition
    Given the expression (select (i32 1) (i32 10) (i32 20))
    Then it should error with "condition must be i1"

  Scenario: Invalid conversion directions
    Given the expression (trunc i64 (i32 42))
    Then it should error with "cannot truncate to larger type"

    Given the expression (zext i8 (i32 42))
    Then it should error with "cannot extend to smaller type"

    Given the expression (sext i16 (i32 42))
    Then it should error with "cannot extend to smaller type"

  Scenario: Float conversion errors
    Given the expression (fptrunc double (float 1.0))
    Then it should error with "cannot truncate to larger type"

    Given the expression (fpext float (double 1.0))
    Then it should error with "cannot extend to smaller type"

  Scenario: Wrong type for int/float conversion
    Given the expression (fptosi double (i32 42))
    Then it should error with "fptosi requires float source"

    Given the expression (sitofp i32 (double 42.0))
    Then it should error with "sitofp requires integer source"
