Feature: Float Arithmetic

  Float arithmetic operations compile to LLVM floating-point instructions.
  Both the dotted syntax (+., -., etc.) and named syntax (fadd, fsub, etc.)
  are supported.

  Scenario: Float addition with +.
    Given the definition (defun test () (+. 1.0 2.0))
    When I evaluate (test)
    Then the float result is 3.0

  Scenario: Float addition with fadd
    Given the definition (defun test () (fadd 1.5 2.5))
    When I evaluate (test)
    Then the float result is 4.0

  Scenario: Float subtraction with -.
    Given the definition (defun test () (-. 5.0 2.0))
    When I evaluate (test)
    Then the float result is 3.0

  Scenario: Float subtraction with fsub
    Given the definition (defun test () (fsub 10.0 3.5))
    When I evaluate (test)
    Then the float result is 6.5

  Scenario: Float multiplication with *.
    Given the definition (defun test () (*. 3.0 4.0))
    When I evaluate (test)
    Then the float result is 12.0

  Scenario: Float multiplication with fmul
    Given the definition (defun test () (fmul 2.5 4.0))
    When I evaluate (test)
    Then the float result is 10.0

  Scenario: Float division with /.
    Given the definition (defun test () (/. 10.0 4.0))
    When I evaluate (test)
    Then the float result is 2.5

  Scenario: Float division with fdiv
    Given the definition (defun test () (fdiv 15.0 2.0))
    When I evaluate (test)
    Then the float result is 7.5

  Scenario: Float remainder with %.
    Given the definition (defun test () (%. 5.0 2.0))
    When I evaluate (test)
    Then the float result is 1.0

  Scenario: Float remainder with frem
    Given the definition (defun test () (frem 7.0 3.0))
    When I evaluate (test)
    Then the float result is 1.0

  Scenario: Nested float operations
    Given the definition (defun test () (+. (*. 2.0 3.0) 1.0))
    When I evaluate (test)
    Then the float result is 7.0

  Scenario: Float in let binding
    Given the definition (defun test () (let ((x 2.5)) (+. x x)))
    When I evaluate (test)
    Then the float result is 5.0

  Scenario: Float passed to function
    Given the definition (defun double (x: double) (+. x x))
    Given the definition (defun test () (double 3.5))
    When I evaluate (test)
    Then the float result is 7.0
