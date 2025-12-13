Feature: Input/Output Operations

  The print and println builtins provide basic output functionality.
  They use printf under the hood and return the number of characters printed.

  Scenario: println returns character count for string
    Given the definition (defun test () (println "hello"))
    When I evaluate (test)
    Then the result is 6

  Scenario: println returns character count for integer
    Given the definition (defun test () (println 42))
    When I evaluate (test)
    Then the result is 3

  Scenario: print returns character count without newline
    Given the definition (defun test () (print "hello"))
    When I evaluate (test)
    Then the result is 5

  Scenario: print returns character count for integer
    Given the definition (defun test () (print 123))
    When I evaluate (test)
    Then the result is 3

  Scenario: Multiple print statements in do block
    Given the definition (defun test () (do (print "a") (println "b") 42))
    When I evaluate (test)
    Then the result is 42

  Scenario: println in let binding
    Given the definition (defun test () (let ((x (println "test"))) x))
    When I evaluate (test)
    Then the result is 5

  Scenario: print negative number
    Given the definition (defun test () (print -99))
    When I evaluate (test)
    Then the result is 3
