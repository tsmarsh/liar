Feature: Pointer Type (ptr)

  LLVM uses opaque pointers. The ptr type represents a pointer without
  specifying what it points to. Typed pointers (like i32*) are deprecated.

  Scenario: Null pointer literal
    Given the expression (ptr null)
    Then the result is (ptr null)

  Scenario: Function returning null pointer
    Given the expression (define (get-null ptr) () (block entry (ret (ptr null))))
    When I call get-null
    Then the result is (ptr null)

  Scenario: Function with pointer parameter
    Given the expression (define (identity-ptr ptr) ((ptr p)) (block entry (ret p)))
    When I call identity-ptr with (ptr null)
    Then the result is (ptr null)
