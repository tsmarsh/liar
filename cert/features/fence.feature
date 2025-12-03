Feature: Memory Fence (Barrier)

  lIR supports fence instructions for explicit memory barriers.
  These prevent reordering of memory operations across the fence.

  Scenario: Fence with seq_cst
    Given the expression (define (test-fence void) () (block entry (fence seq_cst) (ret)))
    Then compilation succeeds

  Scenario: Fence with acquire
    Given the expression (define (test-fence-acq void) () (block entry (fence acquire) (ret)))
    Then compilation succeeds

  Scenario: Fence with release
    Given the expression (define (test-fence-rel void) () (block entry (fence release) (ret)))
    Then compilation succeeds

  Scenario: Fence with monotonic
    Given the expression (define (test-fence-mono void) () (block entry (fence monotonic) (ret)))
    Then compilation succeeds

  Scenario: Fence with acq_rel
    Given the expression (define (test-fence-acqrel void) () (block entry (fence acq_rel) (ret)))
    Then compilation succeeds

  Scenario: Multiple fences
    Given the expression (define (test-multi-fence void) () (block entry (fence acquire) (fence release) (fence seq_cst) (ret)))
    Then compilation succeeds

  Scenario: Fence between memory operations
    Given the expression (define (test-fence-memory i64) () (block entry (let ((p (alloca i64))) (store (i64 42) p) (fence seq_cst) (ret (load i64 p)))))
    When I call test-fence-memory
    Then the result is (i64 42)
