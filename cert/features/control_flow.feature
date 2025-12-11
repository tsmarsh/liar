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

  Scenario: Phi with three incoming edges - negative
    Given the expression (define (classify i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg check-pos)) (block neg (br done)) (block check-pos (br (icmp sgt x (i32 0)) pos zero)) (block pos (br done)) (block zero (br done)) (block done (ret (phi i32 (neg (i32 -1)) (pos (i32 1)) (zero (i32 0))))))
    When I call classify with (i32 -5)
    Then the result is (i32 -1)

  Scenario: Phi with three incoming edges - positive
    Given the expression (define (classify i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg check-pos)) (block neg (br done)) (block check-pos (br (icmp sgt x (i32 0)) pos zero)) (block pos (br done)) (block zero (br done)) (block done (ret (phi i32 (neg (i32 -1)) (pos (i32 1)) (zero (i32 0))))))
    When I call classify with (i32 5)
    Then the result is (i32 1)

  Scenario: Phi with three incoming edges - zero
    Given the expression (define (classify i32) ((i32 x)) (block entry (br (icmp slt x (i32 0)) neg check-pos)) (block neg (br done)) (block check-pos (br (icmp sgt x (i32 0)) pos zero)) (block pos (br done)) (block zero (br done)) (block done (ret (phi i32 (neg (i32 -1)) (pos (i32 1)) (zero (i32 0))))))
    When I call classify with (i32 0)
    Then the result is (i32 0)

  Scenario: Phi with four incoming edges
    Given the expression (define (quarter i32) ((i32 x)) (block entry (br (icmp slt x (i32 25)) q1 check2)) (block q1 (br done)) (block check2 (br (icmp slt x (i32 50)) q2 check3)) (block q2 (br done)) (block check3 (br (icmp slt x (i32 75)) q3 q4)) (block q3 (br done)) (block q4 (br done)) (block done (ret (phi i32 (q1 (i32 1)) (q2 (i32 2)) (q3 (i32 3)) (q4 (i32 4))))))
    When I call quarter with (i32 60)
    Then the result is (i32 3)

  Scenario: Phi with four incoming edges - last quarter
    Given the expression (define (quarter i32) ((i32 x)) (block entry (br (icmp slt x (i32 25)) q1 check2)) (block q1 (br done)) (block check2 (br (icmp slt x (i32 50)) q2 check3)) (block q2 (br done)) (block check3 (br (icmp slt x (i32 75)) q3 q4)) (block q3 (br done)) (block q4 (br done)) (block done (ret (phi i32 (q1 (i32 1)) (q2 (i32 2)) (q3 (i32 3)) (q4 (i32 4))))))
    When I call quarter with (i32 80)
    Then the result is (i32 4)
