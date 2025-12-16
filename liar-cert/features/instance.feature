Feature: Type checking with instance?

  The instance? builtin checks if a value is an instance of a given type
  at runtime by comparing the type_id stored in field 0 of structs.

  Scenario: Check instance of matching type
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (share (Point 10 20)))) (instance? p Point)))
    When I evaluate (test)
    Then the result is 1

  Scenario: Check instance of non-matching type
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defstruct Circle (radius: i64))
    Given the definition (defun test () (let ((p (share (Point 10 20)))) (instance? p Circle)))
    When I evaluate (test)
    Then the result is 0

  Scenario: Check instance with nil returns 0
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (instance? nil Point))
    When I evaluate (test)
    Then the result is 0

  Scenario: Check instance in conditional
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (share (Point 5 10)))) (if (instance? p Point) (. p x) 0)))
    When I evaluate (test)
    Then the result is 5

  Scenario: Multiple types with distinct type_ids
    Given the definition (defstruct A (val: i64))
    Given the definition (defstruct B (val: i64))
    Given the definition (defstruct C (val: i64))
    Given the definition (defun test () (let ((a (share (A 1))) (b (share (B 2))) (c (share (C 3)))) (+ (instance? a A) (+ (instance? b B) (instance? c C)))))
    When I evaluate (test)
    Then the result is 3

  Scenario: Cross-check types
    Given the definition (defstruct A (val: i64))
    Given the definition (defstruct B (val: i64))
    Given the definition (defun test () (let ((a (share (A 1)))) (+ (instance? a A) (instance? a B))))
    When I evaluate (test)
    Then the result is 1
