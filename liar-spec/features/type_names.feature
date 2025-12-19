Feature: Type Names
  Verifies builtin type names compile to the expected lIR types.

  Scenario Outline: Parameter type names
    Given the liar code (defun typed (<param>) <body>)
    When I compile to lIR
    Then compilation succeeds
    And the output contains <lir>

    Examples:
      | param        | body | lir       |
      | x: int       | x    | (i64 x)   |
      | x: i32       | x    | (i32 x)   |
      | x: i16       | x    | (i16 x)   |
      | x: i8        | x    | (i8 x)    |
      | x: float     | x    | (float x) |
      | x: double    | x    | (double x) |
      | x: string    | x    | (ptr x)   |

  Scenario Outline: Return type names
    Given the liar code (defun typed-ret () -> <ret> <body>)
    When I compile to lIR
    Then compilation succeeds
    And the output contains (define (typed-ret <lir>)

    Examples:
      | ret    | body                 | lir    |
      | int    | 1                    | i64    |
      | i32    | (trunc i32 1)        | i32    |
      | i16    | (trunc i16 1)        | i16    |
      | i8     | (trunc i8 1)         | i8     |
      | float  | (fptrunc float 1.0)  | float  |
      | double | 1.0                  | double |
