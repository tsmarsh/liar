Feature: Builtin Array Operations
  Verifies array-related builtins compile to the expected lIR.

  Scenario Outline: Array allocation builtins
    Given the liar expression (<op> 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (array-alloc i64 3)

    Examples:
      | op         |
      | array      |
      | make-array |

  Scenario: Array get builtin
    Given the liar expression (let ((a (array 3))) (aget a 1))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (array-get

  Scenario: Array set builtin
    Given the liar expression (let ((a (array 3))) (aset a 1 42))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (array-set

  Scenario Outline: Array length builtin
    Given the liar expression (<op> (array 3))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (array-len

    Examples:
      | op        |
      | array-len |
      | alen      |

  Scenario: Heap array builtin (literal size)
    Given the liar expression (heap-array 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (heap-array i64 3)

  Scenario: Heap array builtin (dynamic size)
    Given the liar expression (heap-array (+ 1 2))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (heap-array-dyn i64

  Scenario: Array copy builtin
    Given the liar expression (let ((src (heap-array 3)) (dst (heap-array 3))) (array-copy 3 dst src))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (array-copy i64 3

  Scenario: Pointer array allocation builtin
    Given the liar expression (heap-array-ptr 3)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (ptr-array-alloc 3)

  Scenario: Pointer array get builtin
    Given the liar expression (let ((a (heap-array-ptr 3))) (aget-ptr a 1))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (ptr-array-get

  Scenario: Pointer array set builtin
    Given the liar expression (let ((a (heap-array-ptr 3))) (aset-ptr a 1 42))
    When I compile to lIR
    Then compilation succeeds
    And the output contains (ptr-array-set

  Scenario Outline: Array builtins arity (one argument)
    Given the liar expression (<op>)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 1 argument

    Examples:
      | op             |
      | array          |
      | make-array     |
      | heap-array     |
      | heap-array-ptr |
      | array-len      |
      | alen           |

  Scenario Outline: Array builtins arity (two arguments)
    Given the liar expression (<op> 1)
    When I compile to lIR
    Then compilation fails
    And the error contains requires 2 arguments

    Examples:
      | op        |
      | array-get |
      | aget      |
      | aget-ptr  |

  Scenario Outline: Array builtins arity (three arguments)
    Given the liar expression (<op> 1 2)
    When I compile to lIR
    Then compilation fails
    And the error contains requires 3 arguments

    Examples:
      | op        |
      | array-set |
      | aset      |
      | aset-ptr  |
      | array-copy |
