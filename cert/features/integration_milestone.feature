Feature: Integration Milestone (lIR readiness for liar)

  This feature tests comprehensive lIR capabilities. When all scenarios pass,
  lIR is ready for liar to target. Tests are organized in phases:

  Phase 1: Basic Functions
  Phase 2: Control Flow
  Phase 3: Recursion/Loops
  Phase 4: Memory
  Phase 5: External Calls (FFI)
  Phase 6: Structs
  Phase 7: Closure Simulation (ultimate test)

  # ============================================================
  # PHASE 1: Basic Functions
  # ============================================================

  Scenario: Simple function returning constant
    Given the expression (define (get-42 i64) () (block entry (ret (i64 42))))
    When I call get-42
    Then the result is (i64 42)

  Scenario: Function with one parameter
    Given the expression (define (identity i64) ((i64 x)) (block entry (ret x)))
    When I call identity with (i64 99)
    Then the result is (i64 99)

  Scenario: Function with two parameters - addition
    Given the expression (define (add2 i64) ((i64 a) (i64 b)) (block entry (ret (add a b))))
    When I call add2 with (i64 30) (i64 12)
    Then the result is (i64 42)

  Scenario: Function with computation
    Given the expression (define (square i64) ((i64 x)) (block entry (ret (mul x x))))
    When I call square with (i64 7)
    Then the result is (i64 49)

  Scenario: Function calling another function
    Given the expression (define (double i64) ((i64 x)) (block entry (ret (mul x (i64 2)))))
    And the expression (define (quadruple i64) ((i64 x)) (block entry (ret (call @double (call @double x)))))
    When I call quadruple with (i64 5)
    Then the result is (i64 20)

  # ============================================================
  # PHASE 2: Control Flow
  # ============================================================

  Scenario: Unconditional branch between blocks
    Given the expression (define (branch-test i64) () (block entry (br next)) (block next (ret (i64 100))))
    When I call branch-test
    Then the result is (i64 100)

  Scenario: Conditional branch - true path
    Given the expression (define (abs i64) ((i64 x)) (block entry (br (icmp slt x (i64 0)) neg pos)) (block neg (ret (sub (i64 0) x))) (block pos (ret x)))
    When I call abs with (i64 -42)
    Then the result is (i64 42)

  Scenario: Conditional branch - false path
    Given the expression (define (abs i64) ((i64 x)) (block entry (br (icmp slt x (i64 0)) neg pos)) (block neg (ret (sub (i64 0) x))) (block pos (ret x)))
    When I call abs with (i64 42)
    Then the result is (i64 42)

  Scenario: Phi node merging values from two paths
    Given the expression (define (max i64) ((i64 a) (i64 b)) (block entry (br (icmp sgt a b) a_wins b_wins)) (block a_wins (br done)) (block b_wins (br done)) (block done (ret (phi i64 (a_wins a) (b_wins b)))))
    When I call max with (i64 10) (i64 20)
    Then the result is (i64 20)

  # ============================================================
  # PHASE 3: Recursion and Loops
  # ============================================================

  Scenario: Recursive factorial
    Given the expression (define (factorial i64) ((i64 n)) (block entry (br (icmp sle n (i64 1)) base recurse)) (block base (ret (i64 1))) (block recurse (ret (mul n (call @factorial (sub n (i64 1)))))))
    When I call factorial with (i64 5)
    Then the result is (i64 120)

  Scenario: Recursive fibonacci
    Given the expression (define (fib i64) ((i64 n)) (block entry (br (icmp sle n (i64 1)) base recurse)) (block base (ret n)) (block recurse (let ((n1 (call @fib (sub n (i64 1)))) (n2 (call @fib (sub n (i64 2))))) (ret (add n1 n2)))))
    When I call fib with (i64 10)
    Then the result is (i64 55)

  Scenario: Loop with phi - sum 1 to n
    Given the expression (define (sum-to i64) ((i64 n)) (block entry (br loop)) (block loop (let ((i (phi i64 (entry (i64 0)) (loop next-i))) (acc (phi i64 (entry (i64 0)) (loop next-acc)))) (let ((next-i (add i (i64 1))) (next-acc (add acc i))) (br (icmp sle i n) loop done)))) (block done (ret (phi i64 (loop acc)))))
    When I call sum-to with (i64 10)
    Then the result is (i64 55)

  # ============================================================
  # PHASE 4: Memory Operations
  # ============================================================

  Scenario: Stack allocation and load/store
    Given the expression (define (stack-test i64) () (let ((p (alloca i64))) (store (i64 42) p) (ret (load i64 p))))
    When I call stack-test
    Then the result is (i64 42)

  Scenario: Swap via stack
    Given the expression (define (swap-first i64) ((i64 a) (i64 b)) (let ((pa (alloca i64)) (pb (alloca i64))) (store a pa) (store b pb) (let ((tmp (load i64 pa))) (store (load i64 pb) pa) (store tmp pb)) (ret (load i64 pa))))
    When I call swap-first with (i64 10) (i64 20)
    Then the result is (i64 20)

  # ============================================================
  # PHASE 5: External Calls (FFI)
  # ============================================================

  Scenario: External function declaration and call
    Given the expression (declare abs i64 (i64))
    And the expression (define (call-abs i64) ((i64 x)) (ret (call @abs x)))
    When I call call-abs with (i64 -42)
    Then the result is (i64 42)

  # ============================================================
  # PHASE 6: Structs
  # ============================================================

  Scenario: Define struct and access field via GEP
    Given the expression (defstruct point (i64 i64))
    And the expression (define (get-x i64) ((ptr p)) (ret (load i64 (getelementptr %struct.point p (i64 0) (i32 0)))))
    When I call get-x with a point { x: 10, y: 20 }
    Then the result is (i64 10)

  Scenario: Set struct field via GEP
    Given the expression (defstruct counter (i64))
    And the expression (define (set-count void) ((ptr c) (i64 n)) (store n (getelementptr %struct.counter c (i64 0) (i32 0))) (ret))
    When I allocate counter and call set-count with (i64 99)
    Then loading field 0 returns (i64 99)

  # ============================================================
  # PHASE 7: Closure Simulation (Ultimate Test)
  # ============================================================
  # This demonstrates lIR can represent closures via struct + function pointer.
  # The pattern: environment struct + function that takes (env, args...).

  Scenario: Closure simulation - adder
    Given the expression (defstruct adder_env (i64))
    And the expression (define (adder_fn i64) ((ptr env) (i64 y)) (let ((x (load i64 (getelementptr %struct.adder_env env (i64 0) (i32 0))))) (ret (add x y))))
    And the expression (define (make_adder ptr) ((i64 x)) (let ((env (alloca i64))) (store x env) (ret env)))
    And I create an adder with captured value (i64 10)
    When I call the adder with (i64 32)
    Then the result is (i64 42)
