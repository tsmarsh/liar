Feature: Aggregate Types (structs)

  Struct types allow grouping multiple values into a single type.
  Structs are defined with defstruct and referenced by name.

  Scenario: Define a simple struct
    Given the expression (defstruct point (double double))
    Then the struct point is defined

  Scenario: Define a struct with mixed types
    Given the expression (defstruct person (ptr i32 double))
    Then the struct person is defined

  Scenario: Define a struct with integers only
    Given the expression (defstruct coords (i32 i32 i32))
    Then the struct coords is defined
