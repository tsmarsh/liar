Feature: Control Flow (basic blocks, branches, phi)

  LLVM IR uses basic blocks for control flow. Each block has a label
  and contains a sequence of instructions ending with a terminator
  (ret, br, etc).

  Scenario: Single block function
    Given the expression (define (simple i32) () (block entry (ret (i32 42))))
    When I call simple
    Then the result is (i32 42)

  Scenario: Two blocks with unconditional branch
    Given the expression (define (two-blocks i32) () (block entry (br next)) (block next (ret (i32 99))))
    When I call two-blocks
    Then the result is (i32 99)

  Scenario: Conditional branch
    Given the expression (define (cond-br i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg pos)) (block neg (ret (i32 -1))) (block pos (ret (i32 1))))
    When I call cond-br with (i32 -5)
    Then the result is (i32 -1)

  Scenario: Conditional branch takes false path
    Given the expression (define (cond-br i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg pos)) (block neg (ret (i32 -1))) (block pos (ret (i32 1))))
    When I call cond-br with (i32 5)
    Then the result is (i32 1)

  Scenario: Phi node merging values
    Given the expression (define (phi-test i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg pos)) (block neg (br done)) (block pos (br done)) (block done (ret (phi i32 (neg (i32 0)) (pos x)))))
    When I call phi-test with (i32 10)
    Then the result is (i32 10)

  Scenario: Loop with phi
    Given the expression (define (sum-to i32) ((i32 n)) (block entry (br loop)) (block loop (let ((i (phi i32 (entry (i32 0)) (loop next-i))) (acc (phi i32 (entry (i32 0)) (loop next-acc)))) (let ((next-i (add i (i32 1))) (next-acc (add acc i))) (br (icmp sle i n) loop done)))) (block done (ret (phi i32 (loop acc)))))
    When I call sum-to with (i32 5)
    Then the result is (i32 15)
