Feature: Builtin Arithmetic
  Verifies integer and float arithmetic builtins compile to the expected lIR.

  Scenario Outline: Integer arithmetic builtins
    Given the liar expression (<op> 6 7)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op  | lir  |
      | +   | (add |
      | -   | (sub |
      | *   | (mul |
      | /   | (sdiv |
      | rem | (srem |

  Scenario Outline: Float arithmetic builtins
    Given the liar expression (<op> 1.5 2.5)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op   | lir   |
      | +.   | (fadd |
      | -.   | (fsub |
      | *.   | (fmul |
      | /.   | (fdiv |
      | %.   | (frem |
      | fadd | (fadd |
      | fsub | (fsub |
      | fmul | (fmul |
      | fdiv | (fdiv |
      | frem | (frem |

  Scenario Outline: Arithmetic builtins require two arguments
    Given the liar expression (<op> 1)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op   |
      | +    |
      | -    |
      | *    |
      | /    |
      | rem  |
      | +.   |
      | -.   |
      | *.   |
      | /.   |
      | %.   |
      | fadd |
      | fsub |
      | fmul |
      | fdiv |
      | frem |
