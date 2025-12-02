Feature: Function Definitions and Returns
  lIR supports defining functions with the (define ...) form and returning
  values with (ret). This is foundational for building programs.

  Background:
    Functions use the syntax:
    (define (name return-type) ((param-type param-name) ...) body...)

    Ret uses the syntax:
    (ret value)     ; return with value
    (ret)           ; void return

  Scenario: Simple function returning a constant
    Given the expression (define (get-42 i32) () (ret (i32 42)))
    When I call get-42
    Then the result is (i32 42)

  Scenario: Function with one parameter
    Given the expression (define (add-one i32) ((i32 x)) (ret (add x (i32 1))))
    When I call add-one with (i32 5)
    Then the result is (i32 6)

  Scenario: Function with two parameters
    Given the expression (define (add-two i32) ((i32 a) (i32 b)) (ret (add a b)))
    When I call add-two with (i32 3) (i32 4)
    Then the result is (i32 7)

  Scenario: Function returning different integer types
    Given the expression (define (get-i8 i8) () (ret (i8 42)))
    When I call get-i8
    Then the result is (i8 42)

    Given the expression (define (get-i64 i64) () (ret (i64 1000000000000)))
    When I call get-i64
    Then the result is (i64 1000000000000)

  Scenario: Function returning float types
    Given the expression (define (get-pi double) () (ret (double 3.14159)))
    When I call get-pi
    Then the result is (double 3.14159)

    Given the expression (define (get-float float) () (ret (float 2.5)))
    When I call get-float
    Then the result is (float 2.5)

  Scenario: Function with computation in body
    Given the expression (define (square i32) ((i32 x)) (ret (mul x x)))
    When I call square with (i32 7)
    Then the result is (i32 49)

  Scenario: Function with nested expressions
    Given the expression (define (complex i32) ((i32 a) (i32 b)) (ret (add (mul a a) (mul b b))))
    When I call complex with (i32 3) (i32 4)
    Then the result is (i32 25)

  Scenario: Function with float arithmetic
    Given the expression (define (fadd-half double) ((double x)) (ret (fadd x (double 0.5))))
    When I call fadd-half with (double 1.0)
    Then the result is (double 1.5)

  Scenario: Void function (no return value)
    Given the expression (define (void-fn void) () (ret))
    When I call void-fn
    Then the function completes successfully

  Scenario: Function with comparison and select
    Given the expression (define (max i32) ((i32 a) (i32 b)) (ret (select (icmp sgt a b) a b)))
    When I call max with (i32 10) (i32 20)
    Then the result is (i32 20)

    When I call max with (i32 30) (i32 5)
    Then the result is (i32 30)

  Scenario: Function returning boolean
    Given the expression (define (is-positive i1) ((i32 x)) (ret (icmp sgt x (i32 0))))
    When I call is-positive with (i32 5)
    Then the result is (i1 1)

    When I call is-positive with (i32 -5)
    Then the result is (i1 0)
