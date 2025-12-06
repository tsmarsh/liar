Feature: Macros

  Scenario: Simple macro with no transformation
    Given the definition (defmacro identity-macro (x) x)
    Given the definition (defun test () (identity-macro 42))
    When I evaluate (test)
    Then the result is 42

  Scenario: Macro with quasiquote - simple substitution
    Given the definition (defmacro add-one (x) `(+ ,x 1))
    Given the definition (defun test () (add-one 5))
    When I evaluate (test)
    Then the result is 6

  Scenario: Macro expanding to if expression
    Given the definition (defmacro unless (cond then else) `(if ,cond ,else ,then))
    Given the definition (defun test () (unless false 42 0))
    When I evaluate (test)
    Then the result is 42

  Scenario: Macro with multiple unquotes
    Given the definition (defmacro swap-args (a b) `(- ,b ,a))
    Given the definition (defun test () (swap-args 3 10))
    When I evaluate (test)
    Then the result is 7

  Scenario: Nested macro calls
    Given the definition (defmacro double (x) `(+ ,x ,x))
    Given the definition (defmacro quadruple (x) `(double (double ,x)))
    Given the definition (defun test () (quadruple 3))
    When I evaluate (test)
    Then the result is 12

