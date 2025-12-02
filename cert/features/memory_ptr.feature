Feature: Memory Operations with Pointer Types

  Extended memory operations that support ptr type in addition to scalars.
  This allows storing and loading pointers from memory.

  # alloca ptr - allocate space for a pointer
  Scenario: Alloca ptr returns a pointer
    Given the expression (define (alloc-ptr ptr) () (block entry (ret (alloca ptr))))
    When I call alloc-ptr
    Then the result is not null

  # store and load ptr
  Scenario: Store and load a pointer
    Given the expression (define (store-load-ptr ptr) ((ptr input)) (block entry (let ((p (alloca ptr))) (store input p) (ret (load ptr p)))))
    When I call store-load-ptr with (ptr null)
    Then the result is (ptr null)

  Scenario: Store null pointer and load it back
    Given the expression (define (null-roundtrip ptr) () (block entry (let ((p (alloca ptr))) (store (ptr null) p) (ret (load ptr p)))))
    When I call null-roundtrip
    Then the result is (ptr null)

  # Pointer to pointer operations
  Scenario: Double indirection - pointer to pointer
    Given the expression (define (double-ptr i64) ((i64 val)) (block entry (let ((p (alloca i64)) (pp (alloca ptr))) (store val p) (store p pp) (ret (load i64 (load ptr pp))))))
    When I call double-ptr with (i64 42)
    Then the result is (i64 42)
