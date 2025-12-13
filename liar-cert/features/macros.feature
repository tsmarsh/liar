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

  Scenario: Nested macro calls via quasiquote
    Given the definition (defmacro double (x) `(+ ,x ,x))
    Given the definition (defmacro quadruple (x) `(double (double ,x)))
    Given the definition (defun test () (quadruple 3))
    When I evaluate (test)
    Then the result is 12

  Scenario: Macro calling another macro directly
    Given the definition (defmacro double (x) `(+ ,x ,x))
    Given the definition (defmacro quadruple (x) (double (double x)))
    Given the definition (defun test () (quadruple 5))
    When I evaluate (test)
    Then the result is 20

  Scenario: Three levels of macro nesting
    Given the definition (defmacro inc (x) `(+ ,x 1))
    Given the definition (defmacro inc2 (x) (inc (inc x)))
    Given the definition (defmacro inc4 (x) (inc2 (inc2 x)))
    Given the definition (defun test () (inc4 10))
    When I evaluate (test)
    Then the result is 14

  Scenario: Macro with helper macro for conditionals
    Given the definition (defmacro when-positive (x then) `(if (> ,x 0) ,then 0))
    Given the definition (defmacro abs-helper (x) (when-positive x x))
    Given the definition (defun test () (abs-helper 5))
    When I evaluate (test)
    Then the result is 5

  Scenario: Macro using map calls another macro
    Given the definition (defmacro wrap-in-add (x) `(+ ,x 1))
    Given the definition (defmacro add-to-both (a b) `(+ ,(wrap-in-add a) ,(wrap-in-add b)))
    Given the definition (defun test () (add-to-both 3 5))
    When I evaluate (test)
    Then the result is 10

