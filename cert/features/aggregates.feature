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

  # extractvalue - Extract a value from an aggregate

  Scenario: extractvalue - Extract from struct literal
    Given the expression (extractvalue { (i32 10) (i32 20) } 0)
    Then the result is (i32 10)
    Given the expression (extractvalue { (i32 10) (i32 20) } 1)
    Then the result is (i32 20)

  Scenario: extractvalue - Extract double from struct
    Given the expression (extractvalue { (double 1.5) (double 2.5) } 0)
    Then the result is (double 1.5)
    Given the expression (extractvalue { (double 1.5) (double 2.5) } 1)
    Then the result is (double 2.5)

  Scenario: extractvalue - Mixed type struct
    Given the expression (extractvalue { (i32 42) (double 3.14) } 0)
    Then the result is (i32 42)
    Given the expression (extractvalue { (i32 42) (double 3.14) } 1)
    Then the result is (double 3.14)

  # insertvalue - Insert a value into an aggregate
  # We test insertvalue by extracting values to verify they were inserted correctly

  Scenario: insertvalue - Insert into struct and extract
    Given the expression (extractvalue (insertvalue { (i32 1) (i32 2) } (i32 99) 0) 0)
    Then the result is (i32 99)
    Given the expression (extractvalue (insertvalue { (i32 1) (i32 2) } (i32 99) 0) 1)
    Then the result is (i32 2)
    Given the expression (extractvalue (insertvalue { (i32 1) (i32 2) } (i32 99) 1) 0)
    Then the result is (i32 1)
    Given the expression (extractvalue (insertvalue { (i32 1) (i32 2) } (i32 99) 1) 1)
    Then the result is (i32 99)

  Scenario: insertvalue - Insert double and extract
    Given the expression (extractvalue (insertvalue { (double 1.0) (double 2.0) } (double 5.5) 0) 0)
    Then the result is (double 5.5)
    Given the expression (extractvalue (insertvalue { (double 1.0) (double 2.0) } (double 5.5) 0) 1)
    Then the result is (double 2.0)

  Scenario: Chained insertvalue and extract
    Given the expression (extractvalue (insertvalue (insertvalue { (i32 0) (i32 0) } (i32 10) 0) (i32 20) 1) 0)
    Then the result is (i32 10)
    Given the expression (extractvalue (insertvalue (insertvalue { (i32 0) (i32 0) } (i32 10) 0) (i32 20) 1) 1)
    Then the result is (i32 20)
