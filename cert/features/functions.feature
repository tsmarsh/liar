Feature: Function Definitions and Calls

  lIR supports defining functions with the (define ...) form, returning
  values with (ret), and calling functions with (call @name args...).

  Syntax:
    (define (name return-type) ((param-type param-name) ...)
      (block label instructions...))

    (call @function-name arg1 arg2 ...)

  Scenario: Simple function returning a constant
    Given the expression (define (get-42 i32) () (block entry (ret (i32 42))))
    When I call get-42
    Then the result is (i32 42)

  Scenario: Function with one parameter
    Given the expression (define (add-one i32) ((i32 x)) (block entry (ret (add x (i32 1)))))
    When I call add-one with (i32 5)
    Then the result is (i32 6)

  Scenario: Function with two parameters
    Given the expression (define (add-two i32) ((i32 a) (i32 b)) (block entry (ret (add a b))))
    When I call add-two with (i32 3) (i32 4)
    Then the result is (i32 7)

  Scenario: Function returning float
    Given the expression (define (get-pi double) () (block entry (ret (double 3.14159))))
    When I call get-pi
    Then the result is (double 3.14159)

  Scenario: Function with computation
    Given the expression (define (square i32) ((i32 x)) (block entry (ret (mul x x))))
    When I call square with (i32 7)
    Then the result is (i32 49)

  Scenario: Void function
    Given the expression (define (void-fn void) () (block entry (ret)))
    When I call void-fn
    Then the function completes successfully

  Scenario: Call another function
    Given the expression (define (double-it i32) ((i32 x)) (block entry (ret (mul x (i32 2)))))
    And the expression (define (quadruple i32) ((i32 x)) (block entry (ret (call @double-it (call @double-it x)))))
    When I call quadruple with (i32 3)
    Then the result is (i32 12)

  Scenario: Function calling function with different types
    Given the expression (define (to-float double) ((i32 x)) (block entry (ret (sitofp double x))))
    And the expression (define (int-to-float-add double) ((i32 a) (i32 b)) (block entry (ret (fadd (call @to-float a) (call @to-float b)))))
    When I call int-to-float-add with (i32 3) (i32 4)
    Then the result is (double 7.0)

  Scenario: Recursive function (factorial)
    Given the expression (define (factorial i32) ((i32 n)) (block entry (br (icmp sle n (i32 1)) base recurse)) (block base (ret (i32 1))) (block recurse (ret (mul n (call @factorial (sub n (i32 1)))))))
    When I call factorial with (i32 5)
    Then the result is (i32 120)
