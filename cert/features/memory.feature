Feature: Memory Operations (alloca, load, store)

  Memory operations for stack allocation and pointer access.
  Alloca allocates stack space, load reads from memory, store writes to memory.

  Scenario: alloca i32
    Given the expression (define (alloc-i32 ptr) () (block entry (ret (alloca i32))))
    When I call alloc-i32
    Then the result is not null

  Scenario: alloca double
    Given the expression (define (alloc-double ptr) () (block entry (ret (alloca double))))
    When I call alloc-double
    Then the result is not null

  Scenario: alloca with count
    Given the expression (define (alloc-array ptr) () (block entry (ret (alloca i8 (i64 100)))))
    When I call alloc-array
    Then the result is not null

  # Store and load are tested together since we need both to verify correctness
  Scenario: store and load i32
    Given the expression (define (store-load-i32 i32) () (block entry (let ((p (alloca i32))) (store (i32 42) p) (ret (load i32 p)))))
    When I call store-load-i32
    Then the result is (i32 42)

  Scenario: store and load double
    Given the expression (define (store-load-double double) () (block entry (let ((p (alloca double))) (store (double 3.14) p) (ret (load double p)))))
    When I call store-load-double
    Then the result is (double 3.14)

  Scenario: store and load through function parameters
    Given the expression (define (read-ptr i32) ((ptr p)) (block entry (ret (load i32 p))))
    When I call read-ptr with a pointer to (i32 99)
    Then the result is (i32 99)

  # Getelementptr (GEP) - pointer arithmetic
  Scenario: getelementptr on string - simple offset
    Given the expression (getelementptr i8 (string "hello") (i64 0))
    Then the result is a pointer

  Scenario: getelementptr on string - offset by 1
    Given the expression (getelementptr i8 (string "hello") (i64 1))
    Then the result is a pointer

  Scenario: getelementptr inbounds
    Given the expression (getelementptr inbounds i8 (string "hello") (i64 2))
    Then the result is a pointer
