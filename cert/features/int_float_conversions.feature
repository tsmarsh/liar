Feature: Int/Float Conversions (fptoui, fptosi, uitofp, sitofp)
  lIR supports LLVM's conversions between integer and floating point types.
  Signed vs unsigned determines interpretation of integer bits.

  Scenario: fptoui - Float to unsigned integer
    Given the expression (fptoui i32 (double 42.0))
    Then the result is (i32 42)

    Given the expression (fptoui i32 (double 42.9))
    Then the result is (i32 42)

    Given the expression (fptoui i8 (float 255.0))
    Then the result is (i8 -1)

    Given the expression (fptoui i32 (double 0.0))
    Then the result is (i32 0)

  Scenario: fptosi - Float to signed integer
    Given the expression (fptosi i32 (double 42.0))
    Then the result is (i32 42)

    Given the expression (fptosi i32 (double -42.0))
    Then the result is (i32 -42)

    Given the expression (fptosi i32 (double 42.9))
    Then the result is (i32 42)

    Given the expression (fptosi i32 (double -42.9))
    Then the result is (i32 -42)

    Given the expression (fptosi i8 (float 127.0))
    Then the result is (i8 127)

    Given the expression (fptosi i8 (float -128.0))
    Then the result is (i8 -128)

  Scenario: uitofp - Unsigned integer to float
    Given the expression (uitofp double (i32 42))
    Then the result is (double 42.0)

    Given the expression (uitofp double (i8 255))
    Then the result is (double 255.0)

    # Use smaller values that are exactly representable in float
    Given the expression (uitofp float (i16 1000))
    Then the result is (float 1000.0)

    Given the expression (uitofp double (i32 1000000))
    Then the result is (double 1000000.0)

  Scenario: sitofp - Signed integer to float
    Given the expression (sitofp double (i32 42))
    Then the result is (double 42.0)

    Given the expression (sitofp double (i32 -42))
    Then the result is (double -42.0)

    Given the expression (sitofp double (i8 -1))
    Then the result is (double -1.0)

    Given the expression (sitofp float (i32 -1))
    Then the result is (float -1.0)

  Scenario: uitofp vs sitofp with negative integers
    Given the expression (uitofp double (i8 -1))
    Then the result is (double 255.0)

    Given the expression (sitofp double (i8 -1))
    Then the result is (double -1.0)

    Given the expression (uitofp double (i16 -1))
    Then the result is (double 65535.0)

    Given the expression (sitofp double (i16 -1))
    Then the result is (double -1.0)

  Scenario: Float to int truncates toward zero
    Given the expression (fptosi i32 (double 3.7))
    Then the result is (i32 3)

    Given the expression (fptosi i32 (double -3.7))
    Then the result is (i32 -3)

    Given the expression (fptoui i32 (double 3.99))
    Then the result is (i32 3)

  Scenario: Conversions with different float types
    Given the expression (fptosi i32 (float 100.5))
    Then the result is (i32 100)

    Given the expression (sitofp float (i32 1000))
    Then the result is (float 1000.0)

    Given the expression (uitofp float (i16 50000))
    Then the result is (float 50000.0)
