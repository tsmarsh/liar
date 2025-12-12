Feature: Destructuring Let Bindings

  Destructuring allows extracting struct fields directly in let bindings.

  Scenario: Basic struct destructuring
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let (((Point x y) (Point 10 20))) (+ x y)))
    When I evaluate (test)
    Then the result is 30

  Scenario: Destructure and use individual fields
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let (((Point x y) (Point 3 4))) (* x y)))
    When I evaluate (test)
    Then the result is 12

  Scenario: Destructure single field struct
    Given the definition (defstruct Wrapper (value: i64))
    Given the definition (defun test () (let (((Wrapper value) (Wrapper 42))) value))
    When I evaluate (test)
    Then the result is 42

  Scenario: Destructure with expression value
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun make-point (a b) (Point a b))
    Given the definition (defun test () (let (((Point x y) (make-point 5 7))) (+ x y)))
    When I evaluate (test)
    Then the result is 12

  Scenario: Mix destructuring with regular bindings
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((a 1) ((Point x y) (Point 2 3)) (b 4)) (+ a (+ x (+ y b)))))
    When I evaluate (test)
    Then the result is 10

  Scenario: Destructure in nested let
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 10 20))) (let (((Point x y) p)) (- y x))))
    When I evaluate (test)
    Then the result is 10

  Scenario: Multiple destructuring bindings (different structs)
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defstruct Size (w: i64 h: i64))
    Given the definition (defun test () (let (((Point x y) (Point 1 2)) ((Size w h) (Size 3 4))) (+ (+ x w) (+ y h))))
    When I evaluate (test)
    Then the result is 10

  Scenario: Destructure struct with three fields
    Given the definition (defstruct Vec3 (x: i64 y: i64 z: i64))
    Given the definition (defun test () (let (((Vec3 x y z) (Vec3 1 2 3))) (+ x (+ y z))))
    When I evaluate (test)
    Then the result is 6

  Scenario: Use destructured values in computation
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun square (n) (* n n))
    Given the definition (defun test () (let (((Point x y) (Point 3 4))) (+ (square x) (square y))))
    When I evaluate (test)
    Then the result is 25

  Scenario: Destructure function parameter result
    Given the definition (defstruct Pair (fst: i64 snd: i64))
    Given the definition (defun swap (p: Pair) (Pair (. p snd) (. p fst)))
    Given the definition (defun test () (let (((Pair fst snd) (swap (Pair 1 2)))) (- fst snd)))
    When I evaluate (test)
    Then the result is 1
