Feature: Borrow Checker Verification

  The borrow checker enforces memory safety rules for ownership types.
  It runs before codegen and rejects invalid programs.

  Scenario: Valid alloc and drop passes
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (drop x) (ret))))
    Then borrow check passes

  Scenario: Valid multiple shared borrows
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow ref x))) (add (load i64 a) (load i64 b))) (drop x) (ret))))
    Then borrow check passes

  Scenario: Reject use after move
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (let ((y (move x))) (load i64 x)) (ret))))
    Then borrow check fails with "use of moved value"

  Scenario: Reject use after drop
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (drop x) (load i64 x) (ret))))
    Then borrow check fails with "use of dropped value"

  Scenario: Reject double mutable borrow
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow refmut x))) (ret)) (drop x) (ret))))
    Then borrow check fails with "cannot borrow"

  Scenario: Reject mutable borrow when already borrowed
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow refmut x))) (ret)) (drop x) (ret))))
    Then borrow check fails with "cannot borrow"

  Scenario: Reject shared borrow when mutably borrowed
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow ref x))) (ret)) (drop x) (ret))))
    Then borrow check fails with "cannot borrow"

  Scenario: Reject value not dropped
    Given the expression (define (test void) () (block entry (let ((x (alloc own i64))) (ret))))
    Then borrow check fails with "not dropped"
