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

  Scenario: neg of positive
    Given the definition (defun neg (x) (- 0 x))
    Given the definition (defun test () (neg 5))
    When I evaluate (test)
    Then the result is -5

  Scenario: neg of negative
    Given the definition (defun neg (x) (- 0 x))
    Given the definition (defun test () (neg -5))
    When I evaluate (test)
    Then the result is 5

  Scenario: neg of zero
    Given the definition (defun neg (x) (- 0 x))
    Given the definition (defun test () (neg 0))
    When I evaluate (test)
    Then the result is 0

  Scenario: min of different values
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun test () (min 1 2))
    When I evaluate (test)
    Then the result is 1

  Scenario: min when second is smaller
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun test () (min 5 3))
    When I evaluate (test)
    Then the result is 3

  Scenario: min of equal values
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun test () (min 4 4))
    When I evaluate (test)
    Then the result is 4

  Scenario: max of different values
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun test () (max 1 2))
    When I evaluate (test)
    Then the result is 2

  Scenario: max when first is larger
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun test () (max 5 3))
    When I evaluate (test)
    Then the result is 5

  Scenario: max of equal values
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun test () (max 4 4))
    When I evaluate (test)
    Then the result is 4

  Scenario: cube of positive
    Given the definition (defun cube (x) (* x (* x x)))
    Given the definition (defun test () (cube 2))
    When I evaluate (test)
    Then the result is 8

  Scenario: cube of 3
    Given the definition (defun cube (x) (* x (* x x)))
    Given the definition (defun test () (cube 3))
    When I evaluate (test)
    Then the result is 27

  Scenario: cube of negative
    Given the definition (defun cube (x) (* x (* x x)))
    Given the definition (defun test () (cube -2))
    When I evaluate (test)
    Then the result is -8

  Scenario: sign of negative
    Given the definition (defun sign (x) (if (< x 0) -1 (if (> x 0) 1 0)))
    Given the definition (defun test () (sign -5))
    When I evaluate (test)
    Then the result is -1

  Scenario: sign of zero
    Given the definition (defun sign (x) (if (< x 0) -1 (if (> x 0) 1 0)))
    Given the definition (defun test () (sign 0))
    When I evaluate (test)
    Then the result is 0

  Scenario: sign of positive
    Given the definition (defun sign (x) (if (< x 0) -1 (if (> x 0) 1 0)))
    Given the definition (defun test () (sign 5))
    When I evaluate (test)
    Then the result is 1

  Scenario: clamp within range
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun clamp (x lo hi) (max lo (min x hi)))
    Given the definition (defun test () (clamp 5 0 10))
    When I evaluate (test)
    Then the result is 5

  Scenario: clamp below range
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun clamp (x lo hi) (max lo (min x hi)))
    Given the definition (defun test () (clamp -5 0 10))
    When I evaluate (test)
    Then the result is 0

  Scenario: clamp above range
    Given the definition (defun min (a b) (if (< a b) a b))
    Given the definition (defun max (a b) (if (> a b) a b))
    Given the definition (defun clamp (x lo hi) (max lo (min x hi)))
    Given the definition (defun test () (clamp 15 0 10))
    When I evaluate (test)
    Then the result is 10

  Scenario: divisible true
    Given the definition (defun divisible (d n) (= 0 (rem n d)))
    Given the definition (defun test () (divisible 3 9))
    When I evaluate (test)
    Then the result is true

  Scenario: divisible false
    Given the definition (defun divisible (d n) (= 0 (rem n d)))
    Given the definition (defun test () (divisible 3 10))
    When I evaluate (test)
    Then the result is false

  Scenario: in-range true
    Given the definition (defun in-range (x lo hi) (if (<= lo x) (<= x hi) false))
    Given the definition (defun test () (in-range 5 0 10))
    When I evaluate (test)
    Then the result is true

  Scenario: in-range false below
    Given the definition (defun in-range (x lo hi) (if (<= lo x) (<= x hi) false))
    Given the definition (defun test () (in-range -1 0 10))
    When I evaluate (test)
    Then the result is false

  # Math
  # Note: Recursive functions with terminating conditions that involve
  # division/remainder (gcd, lcm) fail due to select evaluating both branches.
  # These need proper branch-based control flow (not yet implemented in liar).
  # factorial, fib, pow, sum-to also fail for same reason.

  # Higher-order functions (require closure support)

  Scenario: constantly returns a function that ignores its argument
    Given the definition (defun constantly (v) (fn (x) v))
    Given the definition (defun test () (let ((always-5 (constantly 5))) (always-5 100)))
    When I evaluate (test)
    Then the result is 5

  Scenario: constantly with zero
    Given the definition (defun constantly (v) (fn (x) v))
    Given the definition (defun test () (let ((always-0 (constantly 0))) (always-0 999)))
    When I evaluate (test)
    Then the result is 0

  Scenario: comp composes two functions
    Given the definition (defun inc (x) (+ x 1))
    Given the definition (defun square (x) (* x x))
    Given the definition (defun comp (f g) (fn (x) (f (g x))))
    Given the definition (defun test () (let ((inc-then-square (comp square inc))) (inc-then-square 4)))
    When I evaluate (test)
    Then the result is 25

  Scenario: comp with identity
    Given the definition (defun identity (x) x)
    Given the definition (defun inc (x) (+ x 1))
    Given the definition (defun comp (f g) (fn (x) (f (g x))))
    Given the definition (defun test () (let ((just-inc (comp identity inc))) (just-inc 5)))
    When I evaluate (test)
    Then the result is 6

