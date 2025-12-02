Feature: Let Bindings (SSA value naming)

  Let bindings allow naming intermediate values in SSA style.
  Values can be referenced by name in subsequent expressions.

  Scenario: Simple let binding - integer
    Given the expression (let ((x (i32 42))) x)
    Then the result is (i32 42)

  Scenario: Simple let binding - double
    Given the expression (let ((x (double 3.14))) x)
    Then the result is (double 3.14)

  Scenario: Let with integer arithmetic
    Given the expression (let ((x (i32 10)) (y (i32 20))) (add x y))
    Then the result is (i32 30)

  Scenario: Let with float arithmetic
    Given the expression (let ((a (double 2.5)) (b (double 4.0))) (fmul a b))
    Then the result is (double 10.0)

  Scenario: Let with comparison
    Given the expression (let ((x (i32 5)) (y (i32 10))) (icmp slt x y))
    Then the result is (i1 1)

  Scenario: Let with select
    Given the expression (let ((x (i32 5)) (y (i32 10))) (select (icmp slt x y) x y))
    Then the result is (i32 5)

  Scenario: Nested let bindings
    Given the expression (let ((x (i32 10))) (let ((y (add x (i32 5)))) y))
    Then the result is (i32 15)

  Scenario: Let binding shadowing
    Given the expression (let ((x (i32 1))) (let ((x (i32 2))) x))
    Then the result is (i32 2)

  Scenario: Let with multiple body expressions
    Given the expression (let ((x (i32 5))) (add x x) (mul x x))
    Then the result is (i32 25)
