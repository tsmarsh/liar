Feature: Macro Compilation
  Verifies that liar macros expand correctly and compile to the expected lIR.
  Macros should be expanded at compile time and not appear in the output.

  Scenario: Simple macro expansion
    Given the liar code:
      """
      (defmacro double (x) `(+ ,x ,x))
      (defun test () (double 21))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (add
    And the output does not contain (defmacro
    And the output does not contain @double

  Scenario: Unless macro with false
    Given the liar code:
      """
      (defmacro unless (cond then else) `(if ,cond ,else ,then))
      (defun test () (unless false 42 0))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 0)
    And the output does not contain (defmacro
    And the output does not contain @unless

  Scenario: Unless macro with true
    Given the liar code:
      """
      (defmacro unless (cond then else) `(if ,cond ,else ,then))
      (defun test () (unless true 42 0))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (i1 1)

  Scenario: Macro with nested quasiquote
    Given the liar code:
      """
      (defmacro add3 (a b c) `(+ ,a (+ ,b ,c)))
      (defun test () (add3 1 2 3))
      """
    When I compile to lIR
    Then compilation succeeds
    And the output contains (add
    And the output does not contain @add3
