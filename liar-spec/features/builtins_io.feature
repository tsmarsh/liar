Feature: Builtin I/O Operations
  Verifies print and println compile to printf calls with correct formats.

  Scenario: Print string
    Given the liar expression (print "hello")
    When I compile to lIR
    Then compilation succeeds
    And the output contains (call @printf
    And the output contains (string "%s")

  Scenario: Print integer
    Given the liar expression (print 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (call @printf
    And the output contains (string "%ld")

  Scenario: Println string
    Given the liar expression (println "hello")
    When I compile to lIR
    Then compilation succeeds
    And the output contains (call @printf
    And the output contains (string "%s\n")

  Scenario: Println integer
    Given the liar expression (println 42)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (call @printf
    And the output contains (string "%ld\n")

  Scenario Outline: I/O builtins require one argument
    Given the liar expression (<op>)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument

    Examples:
      | op      |
      | print   |
      | println |
