#!/bin/bash
# Generate moths for stdlib functions in dependency order
#
# Usage: ./scripts/create-stdlib-moths.sh
#
# This creates .moth/ issues for each stdlib function that needs
# to be implemented, ordered so dependencies come first.
#
# Priority levels:
#   P1 - No dependencies, implement first
#   P2 - Depends on P1 functions
#   P3 - Optimized versions, nice to have
#   P4 - BLOCKED: require runtime primitives not yet in lIR

set -e

MOTH_DIR=".moth"
mkdir -p "$MOTH_DIR"

# Counter for ordering (ensures files sort in dependency order)
ORDER=100

# Generate a speakable 5-char ID (consonant-vowel pattern)
generate_id() {
    local consonants="bcdfghjklmnprstvwxz"
    local vowels="aeiou"
    local id=""
    id+="${consonants:RANDOM%${#consonants}:1}"
    id+="${vowels:RANDOM%${#vowels}:1}"
    id+="${consonants:RANDOM%${#consonants}:1}"
    id+="${vowels:RANDOM%${#vowels}:1}"
    id+="${consonants:RANDOM%${#consonants}:1}"
    echo "$id"
}

# Create a moth file
# Args: priority title description implementation deps tests category
create_moth() {
    local priority="$1"
    local title="$2"
    local description="$3"
    local implementation="$4"
    local deps="$5"
    local tests="$6"
    local category="$7"
    
    local id=$(generate_id)
    local slug="${title// /-}"
    local file="$MOTH_DIR/${id}-stdlib-${slug}.md"
    
    # Avoid collision
    while [ -f "$file" ]; do
        id=$(generate_id)
        file="$MOTH_DIR/${id}-stdlib-${slug}.md"
    done
    
    cat > "$file" << EOF
# stdlib: $title

**Priority:** $priority  
**Category:** stdlib/$category  
**Dependencies:** $deps  
**Order:** $ORDER

## Description

$description

## Implementation

\`\`\`lisp
$implementation
\`\`\`

## Tests

\`\`\`lisp
$tests
\`\`\`

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment
EOF

    echo "[$priority] $title -> $file"
    ORDER=$((ORDER + 10))
}

echo "Creating stdlib moths in dependency order..."
echo "Output directory: $MOTH_DIR/"
echo ""

# ============================================================================
# TIER 1: No dependencies - Basic predicates and simple functions
# ============================================================================

echo "=== Tier 1: No dependencies (P1) ==="

create_moth "P1" "identity" \
    "Returns its argument unchanged. Useful for higher-order functions." \
    "(defun identity (x) x)" \
    "none" \
    "(identity 5)        ; => 5
(identity nil)      ; => nil
(identity true)     ; => true" \
    "core"

create_moth "P1" "zero?" \
    "Returns true if x equals zero." \
    "(defun zero? (x) (= x 0))" \
    "none" \
    "(zero? 0)   ; => true
(zero? 1)   ; => false
(zero? -1)  ; => false" \
    "predicates"

create_moth "P1" "pos?" \
    "Returns true if x is positive (greater than zero)." \
    "(defun pos? (x) (> x 0))" \
    "none" \
    "(pos? 1)   ; => true
(pos? 0)   ; => false
(pos? -1)  ; => false" \
    "predicates"

create_moth "P1" "neg?" \
    "Returns true if x is negative (less than zero)." \
    "(defun neg? (x) (< x 0))" \
    "none" \
    "(neg? -1)  ; => true
(neg? 0)   ; => false
(neg? 1)   ; => false" \
    "predicates"

create_moth "P1" "even?" \
    "Returns true if x is even." \
    "(defun even? (x) (= 0 (rem x 2)))" \
    "none" \
    "(even? 0)  ; => true
(even? 2)  ; => true
(even? 3)  ; => false" \
    "predicates"

create_moth "P1" "odd?" \
    "Returns true if x is odd." \
    "(defun odd? (x) (= 1 (rem x 2)))" \
    "none" \
    "(odd? 1)   ; => true
(odd? 3)   ; => true
(odd? 2)   ; => false" \
    "predicates"

create_moth "P1" "abs" \
    "Returns the absolute value of x." \
    "(defun abs (x) (if (< x 0) (- 0 x) x))" \
    "none" \
    "(abs 5)    ; => 5
(abs -5)   ; => 5
(abs 0)    ; => 0" \
    "arithmetic"

create_moth "P1" "neg" \
    "Returns the negation of x." \
    "(defun neg (x) (- 0 x))" \
    "none" \
    "(neg 5)    ; => -5
(neg -5)   ; => 5
(neg 0)    ; => 0" \
    "arithmetic"

create_moth "P1" "min" \
    "Returns the smaller of two values." \
    "(defun min (a b) (if (< a b) a b))" \
    "none" \
    "(min 1 2)  ; => 1
(min 5 3)  ; => 3
(min 4 4)  ; => 4" \
    "arithmetic"

create_moth "P1" "max" \
    "Returns the larger of two values." \
    "(defun max (a b) (if (> a b) a b))" \
    "none" \
    "(max 1 2)  ; => 2
(max 5 3)  ; => 5
(max 4 4)  ; => 4" \
    "arithmetic"

create_moth "P1" "cube" \
    "Returns x cubed (x * x * x)." \
    "(defun cube (x) (* x (* x x)))" \
    "none" \
    "(cube 2)   ; => 8
(cube 3)   ; => 27
(cube -2)  ; => -8" \
    "arithmetic"

create_moth "P1" "sign" \
    "Returns -1 if x < 0, 0 if x = 0, 1 if x > 0." \
    "(defun sign (x)
  (if (< x 0) -1
      (if (> x 0) 1 0)))" \
    "none" \
    "(sign -5)  ; => -1
(sign 0)   ; => 0
(sign 5)   ; => 1" \
    "arithmetic"

echo ""

# ============================================================================
# TIER 2: Depends on Tier 1
# ============================================================================

echo "=== Tier 2: Depends on P1 (P2) ==="

create_moth "P2" "clamp" \
    "Constrains x to be between lo and hi (inclusive)." \
    "(defun clamp (x lo hi)
  (max lo (min x hi)))" \
    "min, max" \
    "(clamp 5 0 10)   ; => 5
(clamp -5 0 10)  ; => 0
(clamp 15 0 10)  ; => 10" \
    "arithmetic"

create_moth "P2" "divides?" \
    "Returns true if d divides n evenly (n mod d = 0)." \
    "(defun divides? (d n) (= 0 (rem n d)))" \
    "none" \
    "(divides? 2 4)  ; => true
(divides? 2 5)  ; => false
(divides? 3 9)  ; => true" \
    "predicates"

create_moth "P2" "between?" \
    "Returns true if lo <= x <= hi." \
    "(defun between? (x lo hi)
  (and (<= lo x) (<= x hi)))" \
    "none" \
    "(between? 5 0 10)   ; => true
(between? -1 0 10)  ; => false
(between? 10 0 10)  ; => true" \
    "predicates"

create_moth "P2" "gcd" \
    "Greatest common divisor using Euclidean algorithm." \
    "(defun gcd (a b)
  (if (= b 0)
      a
      (gcd b (rem a b))))" \
    "none" \
    "(gcd 12 8)   ; => 4
(gcd 17 5)   ; => 1
(gcd 100 25) ; => 25" \
    "math"

create_moth "P2" "lcm" \
    "Least common multiple." \
    "(defun lcm (a b)
  (/ (* a b) (gcd a b)))" \
    "gcd" \
    "(lcm 4 6)   ; => 12
(lcm 3 5)   ; => 15
(lcm 8 12)  ; => 24" \
    "math"

create_moth "P2" "factorial" \
    "Returns n! (n factorial). n must be non-negative." \
    "(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))" \
    "none" \
    "(factorial 0)  ; => 1
(factorial 1)  ; => 1
(factorial 5)  ; => 120" \
    "math"

create_moth "P2" "fib" \
    "Returns the nth Fibonacci number. Naive recursive implementation." \
    "(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))" \
    "none" \
    "(fib 0)   ; => 0
(fib 1)   ; => 1
(fib 10)  ; => 55" \
    "math"

create_moth "P2" "pow" \
    "Returns base raised to the power exp. exp must be non-negative integer." \
    "(defun pow (base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))))" \
    "none" \
    "(pow 2 0)   ; => 1
(pow 2 10)  ; => 1024
(pow 3 4)   ; => 81" \
    "math"

create_moth "P2" "sum-to" \
    "Returns sum of 1 + 2 + ... + n." \
    "(defun sum-to (n)
  (if (<= n 0)
      0
      (+ n (sum-to (- n 1)))))" \
    "none" \
    "(sum-to 0)    ; => 0
(sum-to 10)   ; => 55
(sum-to 100)  ; => 5050" \
    "math"

create_moth "P2" "comp" \
    "Returns composition of two functions: (comp f g) returns fn that does (f (g x))." \
    "(defun comp (f g)
  (fn (x) (f (g x))))" \
    "none" \
    "(let ((add1-then-square (comp square inc)))
  (add1-then-square 4))  ; => 25" \
    "higher-order"

create_moth "P2" "flip" \
    "Returns a function with arguments flipped: (flip f) returns (fn (a b) (f b a))." \
    "(defun flip (f)
  (fn (a b) (f b a)))" \
    "none" \
    "((flip -) 3 10)  ; => 7  (computes 10 - 3)" \
    "higher-order"

create_moth "P2" "constantly" \
    "Returns a function that always returns v, ignoring its argument." \
    "(defun constantly (v)
  (fn (x) v))" \
    "none" \
    "(let ((always-5 (constantly 5)))
  (always-5 100))  ; => 5" \
    "higher-order"

echo ""

# ============================================================================
# TIER 3: Optimized versions (nice to have)
# ============================================================================

echo "=== Tier 3: Optimized versions (P3) ==="

create_moth "P3" "fib-fast" \
    "Returns the nth Fibonacci number. O(n) iterative via tail recursion." \
    "(defun fib-iter (n a b)
  (if (= n 0)
      a
      (fib-iter (- n 1) b (+ a b))))

(defun fib-fast (n)
  (fib-iter n 0 1))" \
    "none" \
    "(fib-fast 0)    ; => 0
(fib-fast 1)    ; => 1
(fib-fast 50)   ; => 12586269025" \
    "math"

create_moth "P3" "pow-fast" \
    "Fast exponentiation using squaring. O(log n)." \
    "(defun pow-fast (base exp)
  (if (= exp 0)
      1
      (if (even? exp)
          (let ((half (pow-fast base (/ exp 2))))
            (* half half))
          (* base (pow-fast base (- exp 1))))))" \
    "even?" \
    "(pow-fast 2 10)   ; => 1024
(pow-fast 2 20)   ; => 1048576" \
    "math"

create_moth "P3" "comp3" \
    "Composition of three functions: (f (g (h x)))." \
    "(defun comp3 (f g h)
  (fn (x) (f (g (h x)))))" \
    "none" \
    "(let ((f (comp3 square inc inc)))
  (f 3))  ; => 25" \
    "higher-order"

create_moth "P3" "complement" \
    "Returns a function that returns the logical negation of f." \
    "(defun complement (f)
  (fn (x) (not (f x))))" \
    "none" \
    "(let ((not-zero? (complement zero?)))
  (not-zero? 5))  ; => true" \
    "higher-order"

create_moth "P3" "sum-to-fast" \
    "Returns sum of 1 + 2 + ... + n using closed-form formula." \
    "(defun sum-to-fast (n)
  (/ (* n (+ n 1)) 2))" \
    "none" \
    "(sum-to-fast 100)    ; => 5050
(sum-to-fast 1000)   ; => 500500" \
    "math"

echo ""

# ============================================================================
# TIER 4: BLOCKED - Require runtime primitives
# ============================================================================

echo "=== Tier 4: BLOCKED - require runtime primitives (P4) ==="

create_moth "P4" "print" \
    "**BLOCKED: Requires I/O primitives in lIR**

Print a value to stdout without newline. Requires FFI or builtin I/O primitive." \
    ";; BLOCKED: Needs lIR support for extern/FFI
;; Example implementation once available:
;; (extern puts (ptr) i32)
;; (defun print (x) (unsafe (puts (to-cstring x))))" \
    "lIR FFI/extern support" \
    "(print 42)        ; prints: 42
(print \"hello\")  ; prints: hello" \
    "io"

create_moth "P4" "println" \
    "**BLOCKED: Requires I/O primitives in lIR**

Print a value followed by newline to stdout." \
    ";; BLOCKED: Depends on print
;; (defun println (x) (do (print x) (print-char 10)))" \
    "print" \
    "(println 42)      ; prints: 42 then newline" \
    "io"

create_moth "P4" "cons" \
    "**BLOCKED: Requires cons cell runtime**

Creates a pair (cons cell) with car=a and cdr=b. Needs runtime memory allocation for pairs." \
    ";; BLOCKED: Needs struct instantiation at runtime
;; (defstruct Pair (car cdr))
;; (defun cons (a b) (Pair a b))" \
    "runtime struct allocation" \
    "(cons 1 2)           ; => (1 . 2)
(cons 1 (cons 2 nil)) ; => (1 2)" \
    "lists"

create_moth "P4" "car" \
    "**BLOCKED: Requires cons cell runtime**

Returns the first element of a cons cell." \
    ";; (defun car (pair) (. pair car))" \
    "cons" \
    "(car (cons 1 2))  ; => 1" \
    "lists"

create_moth "P4" "cdr" \
    "**BLOCKED: Requires cons cell runtime**

Returns the second element of a cons cell." \
    ";; (defun cdr (pair) (. pair cdr))" \
    "cons" \
    "(cdr (cons 1 2))  ; => 2" \
    "lists"

create_moth "P4" "nil?" \
    "**BLOCKED: Needs nil comparison working**

Returns true if x is nil." \
    ";; May need runtime support for nil comparison
(defun nil? (x) (= x nil))" \
    "nil equality" \
    "(nil? nil)        ; => true
(nil? 0)          ; => false
(nil? [])         ; => false" \
    "predicates"

create_moth "P4" "map" \
    "**BLOCKED: Requires list/vector iteration**

Applies f to each element of coll, returns new collection." \
    ";; BLOCKED: Needs cons/car/cdr or vector iteration
;; (defun map (f coll)
;;   (if (nil? coll)
;;       nil
;;       (cons (f (car coll)) (map f (cdr coll)))))" \
    "cons, car, cdr, nil?" \
    "(map inc [1 2 3])      ; => [2 3 4]
(map square [1 2 3])   ; => [1 4 9]" \
    "collections"

create_moth "P4" "filter" \
    "**BLOCKED: Requires list/vector iteration**

Returns collection of elements where (pred x) is true." \
    ";; BLOCKED: Needs cons/car/cdr or vector iteration
;; (defun filter (pred coll)
;;   (if (nil? coll)
;;       nil
;;       (if (pred (car coll))
;;           (cons (car coll) (filter pred (cdr coll)))
;;           (filter pred (cdr coll)))))" \
    "cons, car, cdr, nil?" \
    "(filter even? [1 2 3 4 5])  ; => [2 4]" \
    "collections"

create_moth "P4" "reduce" \
    "**BLOCKED: Requires list/vector iteration**

Reduces coll to single value by applying (f acc item) left to right." \
    ";; BLOCKED: Needs cons/car/cdr or vector iteration  
;; (defun reduce (f init coll)
;;   (if (nil? coll)
;;       init
;;       (reduce f (f init (car coll)) (cdr coll))))" \
    "cons, car, cdr, nil?" \
    "(reduce + 0 [1 2 3 4 5])  ; => 15
(reduce * 1 [1 2 3 4])    ; => 24" \
    "collections"

create_moth "P4" "length" \
    "**BLOCKED: Requires list/vector iteration**

Returns the number of elements in a collection." \
    ";; BLOCKED: Needs iteration
;; (defun length (coll)
;;   (if (nil? coll)
;;       0
;;       (+ 1 (length (cdr coll)))))" \
    "cons, car, cdr, nil?" \
    "(length [1 2 3])     ; => 3
(length [])          ; => 0" \
    "collections"

create_moth "P4" "reverse" \
    "**BLOCKED: Requires list iteration**

Returns a new list with elements in reverse order." \
    ";; BLOCKED: Needs cons/car/cdr
;; (defun reverse-acc (coll acc)
;;   (if (nil? coll)
;;       acc
;;       (reverse-acc (cdr coll) (cons (car coll) acc))))
;; (defun reverse (coll) (reverse-acc coll nil))" \
    "cons, car, cdr, nil?" \
    "(reverse [1 2 3])  ; => [3 2 1]" \
    "collections"

echo ""
echo "========================================"
echo "Done! Created moths in $MOTH_DIR/"
echo ""
echo "Summary:"
echo "  P1 (no deps):     $(grep -l 'Priority: P1' $MOTH_DIR/*.md 2>/dev/null | wc -l) moths"
echo "  P2 (basic deps):  $(grep -l 'Priority: P2' $MOTH_DIR/*.md 2>/dev/null | wc -l) moths"
echo "  P3 (optimized):   $(grep -l 'Priority: P3' $MOTH_DIR/*.md 2>/dev/null | wc -l) moths"
echo "  P4 (BLOCKED):     $(grep -l 'Priority: P4' $MOTH_DIR/*.md 2>/dev/null | wc -l) moths"
echo ""
echo "To implement in order:"
echo "  1. All P1 functions (no dependencies)"
echo "  2. All P2 functions (depend on P1)"
echo "  3. P3 functions (optimized versions)"
echo "  4. P4 functions require lIR/runtime work first"
echo ""
echo "Quick start - add these to lib/stdlib.liar:"
echo "  grep -h '^(defun' $MOTH_DIR/*-P1-*.md 2>/dev/null || \\"
echo "  for f in \$(grep -l 'Priority: P1' $MOTH_DIR/*.md); do"
echo "    sed -n '/^\`\`\`lisp/,/^\`\`\`/p' \"\$f\" | grep -v '\`\`\`'"
echo "  done"