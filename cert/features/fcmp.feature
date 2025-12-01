Feature: Float Comparison (fcmp)
  lIR supports LLVM's floating point comparison instruction.
  Returns i1. Ordered predicates return false if either operand is NaN.
  Unordered predicates return true if either operand is NaN.

  Scenario: fcmp oeq - Ordered equal
    Given the expression (fcmp oeq (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp oeq (double 1.0) (double 2.0))
    Then the result is (i1 0)

    Given the expression (fcmp oeq (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp one - Ordered not equal
    Given the expression (fcmp one (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp one (double 1.0) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp one (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp olt - Ordered less than
    Given the expression (fcmp olt (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp olt (double 2.0) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp olt (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp ole - Ordered less than or equal
    Given the expression (fcmp ole (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp ole (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp ole (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp ogt - Ordered greater than
    Given the expression (fcmp ogt (double 2.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp ogt (double 1.0) (double 2.0))
    Then the result is (i1 0)

    Given the expression (fcmp ogt (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp oge - Ordered greater than or equal
    Given the expression (fcmp oge (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp oge (double 2.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp oge (double nan) (double 1.0))
    Then the result is (i1 0)

  Scenario: fcmp ord - Ordered (neither is NaN)
    Given the expression (fcmp ord (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp ord (double nan) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp ord (double nan) (double nan))
    Then the result is (i1 0)

  Scenario: fcmp ueq - Unordered or equal
    Given the expression (fcmp ueq (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp ueq (double 1.0) (double 2.0))
    Then the result is (i1 0)

    Given the expression (fcmp ueq (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp une - Unordered or not equal
    Given the expression (fcmp une (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp une (double 1.0) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp une (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp ult - Unordered or less than
    Given the expression (fcmp ult (double 1.0) (double 2.0))
    Then the result is (i1 1)

    Given the expression (fcmp ult (double 2.0) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp ult (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp ule - Unordered or less than or equal
    Given the expression (fcmp ule (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp ule (double 2.0) (double 1.0))
    Then the result is (i1 0)

    Given the expression (fcmp ule (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp ugt - Unordered or greater than
    Given the expression (fcmp ugt (double 2.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp ugt (double 1.0) (double 2.0))
    Then the result is (i1 0)

    Given the expression (fcmp ugt (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp uge - Unordered or greater than or equal
    Given the expression (fcmp uge (double 1.0) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp uge (double 1.0) (double 2.0))
    Then the result is (i1 0)

    Given the expression (fcmp uge (double nan) (double 1.0))
    Then the result is (i1 1)

  Scenario: fcmp uno - Unordered (either is NaN)
    Given the expression (fcmp uno (double nan) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp uno (double 1.0) (double nan))
    Then the result is (i1 1)

    Given the expression (fcmp uno (double 1.0) (double 2.0))
    Then the result is (i1 0)

  Scenario: Infinity comparisons
    Given the expression (fcmp olt (double 1.0) (double inf))
    Then the result is (i1 1)

    Given the expression (fcmp ogt (double inf) (double 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp oeq (double inf) (double inf))
    Then the result is (i1 1)

    Given the expression (fcmp olt (double -inf) (double inf))
    Then the result is (i1 1)

  Scenario: Float type comparisons
    Given the expression (fcmp oeq (float 1.0) (float 1.0))
    Then the result is (i1 1)

    Given the expression (fcmp olt (float 1.5) (float 2.5))
    Then the result is (i1 1)
