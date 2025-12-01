Feature: Select Instruction
  lIR supports LLVM's select instruction for conditional value selection.
  Like a ternary operator: condition ? true_value : false_value

  Scenario: Basic select with i1 condition
    Given the expression (select (i1 1) (i32 10) (i32 20))
    Then the result is (i32 10)

    Given the expression (select (i1 0) (i32 10) (i32 20))
    Then the result is (i32 20)

  Scenario: Select with icmp condition
    Given the expression (select (icmp slt (i32 5) (i32 10)) (i32 1) (i32 2))
    Then the result is (i32 1)

    Given the expression (select (icmp sgt (i32 5) (i32 10)) (i32 1) (i32 2))
    Then the result is (i32 2)

    Given the expression (select (icmp eq (i32 5) (i32 5)) (i64 100) (i64 200))
    Then the result is (i64 100)

  Scenario: Select with fcmp condition
    Given the expression (select (fcmp olt (double 1.0) (double 2.0)) (double 10.0) (double 20.0))
    Then the result is (double 10.0)

    Given the expression (select (fcmp ogt (double 1.0) (double 2.0)) (double 10.0) (double 20.0))
    Then the result is (double 20.0)

  Scenario: Select with integer values
    Given the expression (select (i1 1) (i8 10) (i8 20))
    Then the result is (i8 10)

    Given the expression (select (i1 1) (i16 1000) (i16 2000))
    Then the result is (i16 1000)

    Given the expression (select (i1 0) (i64 999) (i64 111))
    Then the result is (i64 111)

  Scenario: Select with float values
    Given the expression (select (i1 1) (float 1.5) (float 2.5))
    Then the result is (float 1.5)

    Given the expression (select (i1 0) (double 3.14) (double 2.71))
    Then the result is (double 2.71)

  Scenario: Nested select
    Given the expression (select (i1 1) (select (i1 0) (i32 1) (i32 2)) (i32 3))
    Then the result is (i32 2)

    Given the expression (select (i1 0) (i32 1) (select (i1 1) (i32 2) (i32 3)))
    Then the result is (i32 2)

  Scenario: Select with computed values
    Given the expression (select (icmp eq (add (i32 1) (i32 1)) (i32 2)) (i32 100) (i32 200))
    Then the result is (i32 100)

    Given the expression (select (i1 1) (add (i32 10) (i32 20)) (i32 0))
    Then the result is (i32 30)

    Given the expression (select (i1 0) (i32 0) (mul (i32 6) (i32 7)))
    Then the result is (i32 42)

  Scenario: Select preserves type
    Given the expression (select (i1 1) (i32 -1) (i32 1))
    Then the result is (i32 -1)

    Given the expression (select (i1 1) (double -0.0) (double 0.0))
    Then the result is (double -0.0)
