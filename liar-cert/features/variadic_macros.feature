Feature: Variadic Macros

  Macros can accept a variable number of arguments using the rest parameter
  syntax: (defmacro name (x ... rest) body)

  Scenario: Empty rest parameter
    Given the definition (defmacro with-default (x ... rest) (if (nil? rest) x (first rest)))
    Given the definition (defun test () (with-default 42))
    When I evaluate (test)
    Then the result is 42

  Scenario: Rest with one extra arg
    Given the definition (defmacro with-default (x ... rest) (if (nil? rest) x (first rest)))
    Given the definition (defun test () (with-default 42 99))
    When I evaluate (test)
    Then the result is 99

  Scenario: Rest with splicing in quasiquote
    Given the definition (defmacro add-pair (... args) `(+ ,(first args) ,(first (rest args))))
    Given the definition (defun test () (add-pair 10 20))
    When I evaluate (test)
    Then the result is 30

  Scenario: Rest collects multiple args
    Given the definition (defmacro second-of-rest (x ... rest) (if (nil? rest) 0 (if (nil? (rest rest)) (first rest) (first (rest rest)))))
    Given the definition (defun test () (second-of-rest 1 10 20 30))
    When I evaluate (test)
    Then the result is 20

  Scenario: Only rest parameter
    Given the definition (defmacro just-rest (... args) (if (nil? args) 0 (first args)))
    Given the definition (defun test () (just-rest 42 99))
    When I evaluate (test)
    Then the result is 42

  Scenario: Empty only-rest returns nil check
    Given the definition (defmacro count-args (... args) (if (nil? args) 0 1))
    Given the definition (defun test () (count-args))
    When I evaluate (test)
    Then the result is 0

  Scenario: Non-empty only-rest
    Given the definition (defmacro count-args (... args) (if (nil? args) 0 1))
    Given the definition (defun test () (count-args a b c))
    When I evaluate (test)
    Then the result is 1
