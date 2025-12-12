Feature: Type Conversions

  Type conversion operations allow converting between integer widths
  and between integers and floating-point numbers.

  # Integer width conversions

  Scenario: Truncate i64 to i8
    Given the definition (defun test () (trunc i8 255))
    When I evaluate (test)
    Then the result is -1

  Scenario: Zero extend i8 to i64
    Given the definition (defun test () (zext i64 (trunc i8 255)))
    When I evaluate (test)
    Then the result is 255

  Scenario: Sign extend negative i8 to i64
    Given the definition (defun test () (sext i64 (trunc i8 255)))
    When I evaluate (test)
    Then the result is -1

  Scenario: Truncate preserves low bits
    Given the definition (defun test () (zext i64 (trunc i8 256)))
    When I evaluate (test)
    Then the result is 0

  Scenario: Truncate to i16
    Given the definition (defun test () (zext i64 (trunc i16 65535)))
    When I evaluate (test)
    Then the result is 65535

  # Float to int conversions

  Scenario: Float to signed int truncates toward zero
    Given the definition (defun test () (fptosi i64 3.7))
    When I evaluate (test)
    Then the result is 3

  Scenario: Float to signed int with negative
    Given the definition (defun test () (fptosi i64 -3.7))
    When I evaluate (test)
    Then the result is -3

  Scenario: Float to unsigned int
    Given the definition (defun test () (fptoui i64 42.9))
    When I evaluate (test)
    Then the result is 42

  # Int to float conversions

  Scenario: Signed int to double
    Given the definition (defun test () (sitofp double 42))
    When I evaluate (test)
    Then the float result is 42.0

  Scenario: Negative signed int to double
    Given the definition (defun test () (sitofp double -10))
    When I evaluate (test)
    Then the float result is -10.0

  Scenario: Unsigned int to double
    Given the definition (defun test () (uitofp double 100))
    When I evaluate (test)
    Then the float result is 100.0

  # Float precision conversions

  Scenario: Extend float to double
    Given the definition (defun test () (fpext double (fptrunc float 3.5)))
    When I evaluate (test)
    Then the float result is 3.5

  # Combined conversions

  Scenario: Round trip int -> float -> int
    Given the definition (defun test () (fptosi i64 (sitofp double 42)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Convert in function
    Given the definition (defun to-int (x: double) (fptosi i64 x))
    Given the definition (defun test () (to-int 99.9))
    When I evaluate (test)
    Then the result is 99

  Scenario: Convert result of float arithmetic
    Given the definition (defun test () (fptosi i64 (+. 1.5 2.5)))
    When I evaluate (test)
    Then the result is 4
