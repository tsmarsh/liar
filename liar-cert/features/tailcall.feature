Feature: Tail Call Optimization

  Tail calls in tail position should be optimized to avoid stack growth.
  This enables efficient recursion for accumulator-style functions.

  Scenario: Simple direct call in tail position
    Given the definition (defun helper (x) (+ x 1))
    And the definition (defun test () (helper 41))
    When I evaluate (test)
    Then the result is 42

  Scenario: Tail recursive sum with accumulator
    Given the definition (defun sum-to (n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
    And the definition (defun test () (sum-to 10 0))
    When I evaluate (test)
    Then the result is 55

  Scenario: Tail recursive countdown
    Given the definition (defun countdown (n) (if (= n 0) 0 (countdown (- n 1))))
    And the definition (defun test () (countdown 100))
    When I evaluate (test)
    Then the result is 0

  Scenario: Non-tail recursive call (result used in computation)
    Given the definition (defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
    And the definition (defun test () (factorial 5))
    When I evaluate (test)
    Then the result is 120

  Scenario: Multiple function calls with tail call at end
    Given the definition (defun add3 (a b c) (+ a (+ b c)))
    And the definition (defun test () (add3 10 20 30))
    When I evaluate (test)
    Then the result is 60

  Scenario: Deep tail recursion does not overflow stack
    Given the definition (defun deep-recurse (n acc) (if (= n 0) acc (deep-recurse (- n 1) (+ acc 1))))
    And the definition (defun test () (rem (deep-recurse 100000 0) 256))
    When I evaluate (test)
    Then the result is 160
