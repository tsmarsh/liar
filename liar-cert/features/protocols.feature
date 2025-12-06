Feature: Protocol Dispatch

  Scenario: Simple protocol with one method
    Given the definition (defprotocol Greet (greet [self]))
    Given the definition (defstruct Person (name: i64))
    Given the definition (extend-protocol Greet Person (greet [self] (. self name)))
    Given the definition (defun test () (let ((p (Person 42))) (greet p)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Protocol method with extra argument
    Given the definition (defprotocol Addable (add-to [self n]))
    Given the definition (defstruct Counter (value: i64))
    Given the definition (extend-protocol Addable Counter (add-to [self n] (+ (. self value) n)))
    Given the definition (defun test () (let ((c (Counter 10))) (add-to c 5)))
    When I evaluate (test)
    Then the result is 15

  Scenario: Multiple types implementing same protocol
    Given the definition (defprotocol Countable (count [self]))
    Given the definition (defstruct Single (x: i64))
    Given the definition (defstruct Pair (x: i64 y: i64))
    Given the definition (extend-protocol Countable Single (count [self] 1))
    Given the definition (extend-protocol Countable Pair (count [self] 2))
    Given the definition (defun test () (let ((s (Single 1)) (p (Pair 2 3))) (+ (count s) (count p))))
    When I evaluate (test)
    Then the result is 3
