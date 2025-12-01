Feature: Integer Comparison (icmp)
  lIR supports LLVM's integer comparison instruction.
  Returns i1 (0 or 1). Signed vs unsigned predicates differ in how they interpret bits.

  Scenario: icmp eq - Equal
    Given the expression (icmp eq (i32 5) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp eq (i32 5) (i32 6))
    Then the result is (i1 0)

    Given the expression (icmp eq (i8 -1) (i8 255))
    Then the result is (i1 1)

  Scenario: icmp ne - Not equal
    Given the expression (icmp ne (i32 5) (i32 6))
    Then the result is (i1 1)

    Given the expression (icmp ne (i32 5) (i32 5))
    Then the result is (i1 0)

  Scenario: icmp slt - Signed less than
    Given the expression (icmp slt (i32 5) (i32 10))
    Then the result is (i1 1)

    Given the expression (icmp slt (i32 -1) (i32 1))
    Then the result is (i1 1)

    Given the expression (icmp slt (i32 10) (i32 5))
    Then the result is (i1 0)

  Scenario: icmp sle - Signed less than or equal
    Given the expression (icmp sle (i32 5) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp sle (i32 5) (i32 10))
    Then the result is (i1 1)

    Given the expression (icmp sle (i32 10) (i32 5))
    Then the result is (i1 0)

  Scenario: icmp sgt - Signed greater than
    Given the expression (icmp sgt (i32 10) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp sgt (i32 1) (i32 -1))
    Then the result is (i1 1)

    Given the expression (icmp sgt (i32 5) (i32 10))
    Then the result is (i1 0)

  Scenario: icmp sge - Signed greater than or equal
    Given the expression (icmp sge (i32 5) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp sge (i32 10) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp sge (i32 5) (i32 10))
    Then the result is (i1 0)

  Scenario: icmp ult - Unsigned less than
    Given the expression (icmp ult (i32 5) (i32 10))
    Then the result is (i1 1)

    Given the expression (icmp ult (i32 -1) (i32 1))
    Then the result is (i1 0)

    Given the expression (icmp ult (i8 255) (i8 1))
    Then the result is (i1 0)

  Scenario: icmp ule - Unsigned less than or equal
    Given the expression (icmp ule (i32 5) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp ule (i32 5) (i32 10))
    Then the result is (i1 1)

    Given the expression (icmp ule (i8 255) (i8 1))
    Then the result is (i1 0)

  Scenario: icmp ugt - Unsigned greater than
    Given the expression (icmp ugt (i32 10) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp ugt (i32 -1) (i32 1))
    Then the result is (i1 1)

    Given the expression (icmp ugt (i8 255) (i8 0))
    Then the result is (i1 1)

  Scenario: icmp uge - Unsigned greater than or equal
    Given the expression (icmp uge (i32 5) (i32 5))
    Then the result is (i1 1)

    Given the expression (icmp uge (i8 255) (i8 1))
    Then the result is (i1 1)

    Given the expression (icmp uge (i32 1) (i32 -1))
    Then the result is (i1 0)

  Scenario: Comparisons across integer widths
    Given the expression (icmp eq (i8 42) (i8 42))
    Then the result is (i1 1)

    Given the expression (icmp eq (i16 1000) (i16 1000))
    Then the result is (i1 1)

    Given the expression (icmp eq (i64 9223372036854775807) (i64 9223372036854775807))
    Then the result is (i1 1)

  Scenario: Signed vs unsigned with negative numbers
    Given the expression (icmp slt (i8 -1) (i8 0))
    Then the result is (i1 1)

    Given the expression (icmp ult (i8 -1) (i8 0))
    Then the result is (i1 0)

    Given the expression (icmp sgt (i8 0) (i8 -1))
    Then the result is (i1 1)

    Given the expression (icmp ugt (i8 0) (i8 -1))
    Then the result is (i1 0)
