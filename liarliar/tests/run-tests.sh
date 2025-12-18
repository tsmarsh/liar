#!/bin/bash
# liarliar integration tests
# Tests the self-hosted compiler against known-good cases

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LIARLIAR="${LIARLIAR:-/tmp/liarliar}"
LAIR="$REPO_ROOT/target/release/lair"
TMP_DIR="${TMP_DIR:-/tmp/liarliar-tests}"

mkdir -p "$TMP_DIR"

PASSED=0
FAILED=0

run_test() {
    local name="$1"
    local source="$2"
    local expected="$3"
    
    local liar_file="$TMP_DIR/$name.liar"
    local lir_file="$TMP_DIR/$name.lir"
    local bin_file="$TMP_DIR/$name"
    
    echo -n "  $name... "
    
    echo "$source" > "$liar_file"
    
    if ! "$LIARLIAR" "$liar_file" > "$lir_file" 2>&1; then
        echo "FAIL (liarliar crashed)"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if ! "$LAIR" "$lir_file" -o "$bin_file" 2>/dev/null; then
        echo "FAIL (lair failed)"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    "$bin_file"
    local exit_code=$?
    
    if [ "$exit_code" = "$expected" ]; then
        echo "OK"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL (expected $expected, got $exit_code)"
        FAILED=$((FAILED + 1))
    fi
}

echo "=== liarliar integration tests ==="
echo ""

echo "Basic expressions:"
run_test "literal-int" '(defun main () -> i64 42)' 42
run_test "addition" '(defun main () -> i64 (+ 1 2))' 3
run_test "subtraction" '(defun main () -> i64 (- 10 3))' 7
run_test "multiplication" '(defun main () -> i64 (* 6 7))' 42
run_test "division" '(defun main () -> i64 (/ 100 10))' 10
run_test "nested-arith" '(defun main () -> i64 (+ (* 3 4) (- 10 5)))' 17

echo ""
echo "Let bindings:"
run_test "let-single" '(defun main () -> i64 (let ((x 42)) x))' 42
run_test "let-multiple" '(defun main () -> i64 (let ((x 10) (y 20)) (+ x y)))' 30
run_test "let-nested" '(defun main () -> i64 (let ((x 5)) (let ((y 10)) (+ x y))))' 15

echo ""
echo "Conditionals (pure):"
run_test "if-true" '(defun main () -> i64 (if (< 1 2) 10 20))' 10
run_test "if-false" '(defun main () -> i64 (if (> 1 2) 10 20))' 20
run_test "if-eq" '(defun main () -> i64 (if (= 5 5) 1 0))' 1
run_test "if-lte" '(defun main () -> i64 (if (<= 5 5) 1 0))' 1
run_test "if-gte" '(defun main () -> i64 (if (>= 5 5) 1 0))' 1

echo ""
echo "Function calls:"
run_test "fn-noargs" '(defun answer () -> i64 42) (defun main () -> i64 (answer))' 42
run_test "fn-onearg" '(defun double (x: i64) -> i64 (* x 2)) (defun main () -> i64 (double 21))' 42
run_test "fn-twoargs" '(defun add (a: i64 b: i64) -> i64 (+ a b)) (defun main () -> i64 (add 19 23))' 42
run_test "fn-chain" '(defun inc (x: i64) -> i64 (+ x 1)) (defun main () -> i64 (inc (inc (inc 39))))' 42

echo ""
echo "Combined:"
run_test "fn-with-let" '(defun calc (x: i64) -> i64 (let ((y (* x 2))) (+ y 1))) (defun main () -> i64 (calc 20))' 41
run_test "fn-with-if" '(defun max (a: i64 b: i64) -> i64 (if (> a b) a b)) (defun main () -> i64 (max 10 42))' 42

echo ""
echo "Recursive functions:"
run_test "recursive-countdown" '
(defun countdown (n: i64) -> i64
  (if (<= n 0)
      0
      (countdown (- n 1))))
(defun main () -> i64 (countdown 10))' 0

run_test "recursive-sum" '
(defun sum-to (n: i64) -> i64
  (if (<= n 0)
      0
      (+ n (sum-to (- n 1)))))
(defun main () -> i64 (sum-to 10))' 55

run_test "recursive-fib" '
(defun fib (n: i64) -> i64
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(defun main () -> i64 (fib 10))' 55

run_test "mutual-recursion" '
(defun is-even (n: i64) -> i64
  (if (= n 0) 1 (is-odd (- n 1))))
(defun is-odd (n: i64) -> i64
  (if (= n 0) 0 (is-even (- n 1))))
(defun main () -> i64 (is-even 10))' 1

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="

if [ $FAILED -gt 0 ]; then
    exit 1
fi
