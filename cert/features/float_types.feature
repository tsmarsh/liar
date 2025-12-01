Feature: Float Types (float, double)
  lIR supports LLVM's floating point types: float (32-bit) and double (64-bit).
  No f32/f64 aliases—use float and double exactly as LLVM does.

  Scenario: float literals
    Given the expression (float 0.0)
    Then the result is (float 0.0)

    Given the expression (float 1.0)
    Then the result is (float 1.0)

    Given the expression (float -1.0)
    Then the result is (float -1.0)

    Given the expression (float 3.14159)
    Then the result is (float 3.14159)

  Scenario: double literals
    Given the expression (double 0.0)
    Then the result is (double 0.0)

    Given the expression (double 1.0)
    Then the result is (double 1.0)

    Given the expression (double -1.0)
    Then the result is (double -1.0)

    Given the expression (double 3.141592653589793)
    Then the result is (double 3.141592653589793)

  Scenario: Scientific notation
    Given the expression (float 1.0e10)
    Then the result is (float 1.0e10)

    Given the expression (float 1.5e-5)
    Then the result is (float 1.5e-5)

    Given the expression (double 1.0e100)
    Then the result is (double 1.0e100)

    Given the expression (double 2.5e-300)
    Then the result is (double 2.5e-300)

  Scenario: Positive and negative zero
    Given the expression (float 0.0)
    Then the result is (float 0.0)

    Given the expression (float -0.0)
    Then the result is (float -0.0)

    Given the expression (double 0.0)
    Then the result is (double 0.0)

    Given the expression (double -0.0)
    Then the result is (double -0.0)

  Scenario: Infinity
    Given the expression (float inf)
    Then the result is (float inf)

    Given the expression (float -inf)
    Then the result is (float -inf)

    Given the expression (double inf)
    Then the result is (double inf)

    Given the expression (double -inf)
    Then the result is (double -inf)

  Scenario: NaN (Not a Number)
    Given the expression (float nan)
    Then the result is (float nan)

    Given the expression (double nan)
    Then the result is (double nan)
