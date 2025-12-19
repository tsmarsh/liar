Feature: Builtin Conversions
  Verifies scalar type conversions compile to the expected lIR and validate inputs.

  Scenario Outline: Conversion builtins
    Given the liar expression (<op> <ty> <val>)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | op      | ty     | val                     | lir          |
      | trunc   | i8     | 255                     | (trunc i8    |
      | trunc   | i1     | 1                       | (trunc i1    |
      | trunc   | i16    | 255                     | (trunc i16   |
      | trunc   | i32    | 255                     | (trunc i32   |
      | zext    | i64    | (trunc i8 1)            | (zext i64    |
      | sext    | i64    | (trunc i8 255)          | (sext i64    |
      | fptrunc | float  | 1.5                     | (fptrunc float |
      | fpext   | double | (fptrunc float 1.5)     | (fpext double |
      | fptosi  | i64    | 1.5                     | (fptosi i64  |
      | fptoui  | i64    | 1.5                     | (fptoui i64  |
      | sitofp  | double | 42                      | (sitofp double |
      | uitofp  | double | 42                      | (uitofp double |

  Scenario Outline: Conversion builtins require two arguments
    Given the liar expression (<op> i64)
    When I compile to lIR
    Then compilation fails
    And the error contains requires exactly 2 arguments

    Examples:
      | op      |
      | trunc   |
      | zext    |
      | sext    |
      | fptrunc |
      | fpext   |
      | fptosi  |
      | fptoui  |
      | sitofp  |
      | uitofp  |

  Scenario: Conversion builtins require a type name
    Given the liar expression (trunc 1 2)
    When I compile to lIR
    Then compilation fails
    And the error contains type conversion requires a type name

  Scenario: Conversion builtins reject unknown types
    Given the liar expression (trunc i128 1)
    When I compile to lIR
    Then compilation fails
    And the error contains undefined variable
