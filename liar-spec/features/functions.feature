Feature: Function Compilation
  Verifies that liar functions compile to the correct lIR representation.

  Scenario: Function with no parameters
    Given the liar code (defun test () 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (define (test i64)
    And the output contains (ret (i64 42))

  Scenario: Function with one parameter
    Given the liar code (defun identity (x) x)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (define (identity i64)
    And the output contains (ret

  Scenario: Function with typed parameter
    Given the liar code (defun typed (x: i64) x)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i64 x)

  Scenario: Function with return type annotation
    Given the liar code (defun annotated () -> i64 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (define (annotated i64)

  Scenario: Recursive function call
    Given the liar code:
      """
      (defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))
      (defun test () (fact 5))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (call @fact
    And the lIR parses

  Scenario: Function parameter call lowers to indirect-call
    Given the liar code:
      """
      (defun apply1 (f x) (f x))
      (defun inc (x) (+ x 1))
      (defun test () (apply1 inc 1))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (indirect-tailcall
    And the lIR parses
