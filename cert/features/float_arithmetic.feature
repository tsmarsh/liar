Feature: Float Arithmetic (fadd, fsub, fmul, fdiv, frem)
  lIR supports LLVM's floating point arithmetic operations.
  Operations require matching types—no automatic promotion.

  Scenario: fadd - Float addition
    Given the expression (fadd (float 1.5) (float 2.5))
    Then the result is (float 4.0)

    Given the expression (fadd (double 1.5) (double 2.5))
    Then the result is (double 4.0)

    Given the expression (fadd (double 0.1) (double 0.2))
    Then the result is (double 0.30000000000000004)

  Scenario: fsub - Float subtraction
    Given the expression (fsub (float 5.0) (float 3.0))
    Then the result is (float 2.0)

    Given the expression (fsub (double 10.5) (double 3.25))
    Then the result is (double 7.25)

    Given the expression (fsub (float 1.0) (float 1.0))
    Then the result is (float 0.0)

  Scenario: fmul - Float multiplication
    Given the expression (fmul (float 3.0) (float 4.0))
    Then the result is (float 12.0)

    Given the expression (fmul (double 2.5) (double 4.0))
    Then the result is (double 10.0)

    Given the expression (fmul (double -2.0) (double 3.0))
    Then the result is (double -6.0)

  Scenario: fdiv - Float division
    Given the expression (fdiv (float 10.0) (float 4.0))
    Then the result is (float 2.5)

    Given the expression (fdiv (double 7.0) (double 2.0))
    Then the result is (double 3.5)

    Given the expression (fdiv (double 1.0) (double 3.0))
    Then the result is (double 0.3333333333333333)

  Scenario: frem - Float remainder
    Given the expression (frem (float 10.0) (float 3.0))
    Then the result is (float 1.0)

    Given the expression (frem (double 7.5) (double 2.5))
    Then the result is (double 0.0)

    Given the expression (frem (double -10.0) (double 3.0))
    Then the result is (double -1.0)

  Scenario: Infinity arithmetic
    Given the expression (fadd (double inf) (double 1.0))
    Then the result is (double inf)

    Given the expression (fsub (double inf) (double inf))
    Then the result is (double nan)

    Given the expression (fmul (double inf) (double 2.0))
    Then the result is (double inf)

    Given the expression (fmul (double inf) (double 0.0))
    Then the result is (double nan)

    Given the expression (fdiv (double 1.0) (double 0.0))
    Then the result is (double inf)

    Given the expression (fdiv (double -1.0) (double 0.0))
    Then the result is (double -inf)

  Scenario: NaN propagation
    Given the expression (fadd (double nan) (double 1.0))
    Then the result is (double nan)

    Given the expression (fsub (double 1.0) (double nan))
    Then the result is (double nan)

    Given the expression (fmul (double nan) (double nan))
    Then the result is (double nan)

    Given the expression (fdiv (double nan) (double 1.0))
    Then the result is (double nan)

  Scenario: Signed zero
    Given the expression (fadd (double -0.0) (double 0.0))
    Then the result is (double 0.0)

    Given the expression (fmul (double -1.0) (double 0.0))
    Then the result is (double -0.0)

    Given the expression (fdiv (double 1.0) (double -inf))
    Then the result is (double -0.0)
