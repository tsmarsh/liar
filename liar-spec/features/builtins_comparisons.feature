Feature: Builtin Comparisons
  Verifies integer and float comparison builtins compile to the expected lIR.

  Scenario Outline: Integer comparison builtins
    Given the liar code (defun test () (if (<op> 1 2) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op | lir |
      | =  | (icmp eq |
      | == | (icmp eq |
      | != | (icmp ne |
      | <  | (icmp slt |
      | >  | (icmp sgt |
      | <= | (icmp sle |
      | >= | (icmp sge |

  Scenario Outline: Float comparison builtins
    Given the liar code (defun test () (if (<op> 1.0 2.0) 1 0))
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op  | lir |
      | =.  | (fcmp oeq |
      | f=  | (fcmp oeq |
      | !=. | (fcmp one |
      | f!= | (fcmp one |
      | <.  | (fcmp olt |
      | f<  | (fcmp olt |
      | >.  | (fcmp ogt |
      | f>  | (fcmp ogt |
      | <=. | (fcmp ole |
      | f<= | (fcmp ole |
      | >=. | (fcmp oge |
      | f>= | (fcmp oge |

  Scenario Outline: Comparison builtins require two arguments
    Given the liar expression (<op> 1)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op  |
      | =   |
      | ==  |
      | !=  |
      | <   |
      | >   |
      | <=  |
      | >=  |
      | =.  |
      | f=  |
      | !=. |
      | f!= |
      | <.  |
      | f<  |
      | >.  |
      | f>  |
      | <=. |
      | f<= |
      | >=. |
      | f>= |
