Feature: String Constants

  String literals in lIR are null-terminated character arrays, represented
  as constant [N x i8] arrays. When used in expressions, they return a
  pointer to the first character.

  Scenario: Simple string literal
    Given the expression (string "hello")
    Then the result is a pointer

  Scenario: String with escape sequences
    Given the expression (string "line1\nline2")
    Then the result is a pointer

  Scenario: String with tab escape
    Given the expression (string "col1\tcol2")
    Then the result is a pointer

  Scenario: String with null escape
    Given the expression (string "before\0after")
    Then the result is a pointer

  Scenario: String with backslash escape
    Given the expression (string "path\\to\\file")
    Then the result is a pointer

  Scenario: String with quote escape
    Given the expression (string "say \"hello\"")
    Then the result is a pointer

  Scenario: Empty string
    Given the expression (string "")
    Then the result is a pointer
