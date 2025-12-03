Feature: Tail Call Optimization

  lIR supports guaranteed tail call optimization with the (tailcall ...) form.
  Tail calls reuse the current stack frame, enabling efficient recursion without
  stack growth.

  Syntax:
    (tailcall @function-name arg1 arg2 ...)

  The tailcall instruction:
  - Must be in tail position (followed by implicit return)
  - Reuses the current stack frame
  - Return type must match the caller's return type

  Scenario: Tail recursive factorial
    Given the expression (define (fact-tail i64) ((i64 n) (i64 acc)) (block entry (br (icmp eq n (i64 0)) done recurse)) (block done (ret acc)) (block recurse (tailcall @fact-tail (sub n (i64 1)) (mul n acc))))
    When I call fact-tail with (i64 5) (i64 1)
    Then the result is (i64 120)

  Scenario: Tail recursive countdown
    Given the expression (define (countdown i64) ((i64 n)) (block entry (br (icmp eq n (i64 0)) done recurse)) (block done (ret (i64 0))) (block recurse (tailcall @countdown (sub n (i64 1)))))
    When I call countdown with (i64 1000)
    Then the result is (i64 0)

  Scenario: Tail call to different function
    Given the expression (define (helper i32) ((i32 x)) (block entry (ret (mul x (i32 2)))))
    And the expression (define (caller i32) ((i32 x)) (block entry (tailcall @helper (add x (i32 1)))))
    When I call caller with (i32 5)
    Then the result is (i32 12)

  Scenario: Tail recursive sum
    Given the expression (define (sum-tail i64) ((i64 n) (i64 acc)) (block entry (br (icmp eq n (i64 0)) done recurse)) (block done (ret acc)) (block recurse (tailcall @sum-tail (sub n (i64 1)) (add acc n))))
    When I call sum-tail with (i64 10) (i64 0)
    Then the result is (i64 55)
