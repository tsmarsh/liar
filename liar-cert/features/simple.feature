Feature: Core Language Tests

  Scenario: Simple addition
    Given the definition (defun test () (+ 1 2))
    When I evaluate (test)
    Then the result is 3

  Scenario: Simple subtraction
    Given the definition (defun test () (- 10 3))
    When I evaluate (test)
    Then the result is 7

  Scenario: Simple multiplication
    Given the definition (defun test () (* 6 7))
    When I evaluate (test)
    Then the result is 42

  Scenario: Integer division
    Given the definition (defun test () (/ 20 4))
    When I evaluate (test)
    Then the result is 5

  Scenario: Remainder
    Given the definition (defun test () (rem 17 5))
    When I evaluate (test)
    Then the result is 2

  Scenario: Let binding
    Given the definition (defun test () (let ((x 10)) x))
    When I evaluate (test)
    Then the result is 10

  Scenario: Pointer array store and retrieve
    Given the definition (defstruct Box (value: i64))
    Given the definition (defun test () (let ((arr (heap-array-ptr 3)) (b (share (Box 42))) (s (aset-ptr arr 0 b))) (. (aget-ptr arr 0) value)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Pointer array multiple values
    Given the definition (defstruct Box (value: i64))
    Given the definition (defun test () (let ((arr (heap-array-ptr 3)) (b1 (share (Box 10))) (b2 (share (Box 20))) (b3 (share (Box 30))) (s1 (aset-ptr arr 0 b1)) (s2 (aset-ptr arr 1 b2)) (s3 (aset-ptr arr 2 b3))) (+ (. (aget-ptr arr 0) value) (+ (. (aget-ptr arr 1) value) (. (aget-ptr arr 2) value)))))
    When I evaluate (test)
    Then the result is 60
