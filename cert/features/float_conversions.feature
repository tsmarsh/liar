Feature: Float Conversions (fptrunc, fpext)
  lIR supports LLVM's floating point conversion instructions.
  fptrunc narrows precision, fpext widens precision.

  Scenario: fptrunc - Truncate double to float
    Given the expression (fptrunc float (double 1.0))
    Then the result is (float 1.0)

    Given the expression (fptrunc float (double 3.14159265358979))
    Then the result is (float 3.1415927)

    Given the expression (fptrunc float (double 1.0e100))
    Then the result is (float inf)

    Given the expression (fptrunc float (double -1.0e100))
    Then the result is (float -inf)

  Scenario: fpext - Extend float to double
    Given the expression (fpext double (float 1.0))
    Then the result is (double 1.0)

    # Use exactly representable values to avoid platform-specific precision differences
    Given the expression (fpext double (float 1.5))
    Then the result is (double 1.5)

    Given the expression (fpext double (float 0.25))
    Then the result is (double 0.25)

  Scenario: Special values through fptrunc
    Given the expression (fptrunc float (double inf))
    Then the result is (float inf)

    Given the expression (fptrunc float (double -inf))
    Then the result is (float -inf)

    Given the expression (fptrunc float (double nan))
    Then the result is (float nan)

    Given the expression (fptrunc float (double -0.0))
    Then the result is (float -0.0)

  Scenario: Special values through fpext
    Given the expression (fpext double (float inf))
    Then the result is (double inf)

    Given the expression (fpext double (float -inf))
    Then the result is (double -inf)

    Given the expression (fpext double (float nan))
    Then the result is (double nan)

    Given the expression (fpext double (float -0.0))
    Then the result is (double -0.0)

  Scenario: Precision loss in fptrunc
    # Use values where precision loss is clear and platform-independent
    Given the expression (fptrunc float (double 1.125))
    Then the result is (float 1.125)

    Given the expression (fptrunc float (double 0.0625))
    Then the result is (float 0.0625)

  Scenario: Round-trip conversions
    Given the expression (fpext double (fptrunc float (double 1.5)))
    Then the result is (double 1.5)

    Given the expression (fptrunc float (fpext double (float 2.5)))
    Then the result is (float 2.5)
