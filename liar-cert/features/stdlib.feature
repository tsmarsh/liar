Feature: Standard Library

  # Core

  Scenario: identity returns its argument
    Given the definition (defun identity (x) x)
    Given the definition (defun test () (identity 5))
    When I evaluate (test)
    Then the result is 5

  # Predicates

  Scenario: zerop true for zero
    Given the definition (defun zerop (x) (= x 0))
    Given the definition (defun test () (zerop 0))
    When I evaluate (test)
    Then the result is true

  Scenario: zerop false for non-zero
    Given the definition (defun zerop (x) (= x 0))
    Given the definition (defun test () (zerop 5))
    When I evaluate (test)
    Then the result is false

  Scenario: posp true for positive
    Given the definition (defun posp (x) (> x 0))
    Given the definition (defun test () (posp 5))
    When I evaluate (test)
    Then the result is true

  Scenario: posp false for zero
    Given the definition (defun posp (x) (> x 0))
    Given the definition (defun test () (posp 0))
    When I evaluate (test)
    Then the result is false

  Scenario: negp true for negative
    Given the definition (defun negp (x) (< x 0))
    Given the definition (defun test () (negp -5))
    When I evaluate (test)
    Then the result is true

  Scenario: negp false for positive
    Given the definition (defun negp (x) (< x 0))
    Given the definition (defun test () (negp 5))
    When I evaluate (test)
    Then the result is false

  Scenario: evenp true for even
    Given the definition (defun evenp (x) (= 0 (rem x 2)))
    Given the definition (defun test () (evenp 4))
    When I evaluate (test)
    Then the result is true

  Scenario: evenp false for odd
    Given the definition (defun evenp (x) (= 0 (rem x 2)))
    Given the definition (defun test () (evenp 5))
    When I evaluate (test)
    Then the result is false

  Scenario: oddp true for odd
    Given the definition (defun oddp (x) (= 1 (rem x 2)))
    Given the definition (defun test () (oddp 5))
    When I evaluate (test)
    Then the result is true

  Scenario: oddp false for even
    Given the definition (defun oddp (x) (= 1 (rem x 2)))
    Given the definition (defun test () (oddp 4))
    When I evaluate (test)
    Then the result is false

  # Arithmetic

  Scenario: abs of positive
    Given the definition (defun abs (x) (if (< x 0) (- 0 x) x))
    Given the definition (defun test () (abs 5))
    When I evaluate (test)
    Then the result is 5

  Scenario: abs of negative
    Given the definition (defun abs (x) (if (< x 0) (- 0 x) x))
    Given the definition (defun test () (abs -5))
    When I evaluate (test)
    Then the result is 5

  Scenario: abs of zero
    Given the definition (defun abs (x) (if (< x 0) (- 0 x) x))
    Given the definition (defun test () (abs 0))
    When I evaluate (test)
    Then the result is 0
