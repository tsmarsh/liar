Feature: Protocol Dispatch

  Scenario: Simple protocol with one method
    Given the definition (defprotocol Greet (greet [self]))
    Given the definition (defstruct Person (name: i64))
    Given the definition (extend-protocol Greet Person (greet [self] (. self name)))
    Given the definition (defun test () (let ((p (Person 42))) (greet p)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Protocol method with extra argument
    Given the definition (defprotocol Addable (add-to [self n]))
    Given the definition (defstruct Counter (value: i64))
    Given the definition (extend-protocol Addable Counter (add-to [self n] (+ (. self value) n)))
    Given the definition (defun test () (let ((c (Counter 10))) (add-to c 5)))
    When I evaluate (test)
    Then the result is 15

  Scenario: Multiple types implementing same protocol
    Given the definition (defprotocol Countable (count [self]))
    Given the definition (defstruct Single (x: i64))
    Given the definition (defstruct Pair (x: i64 y: i64))
    Given the definition (extend-protocol Countable Single (count [self] 1))
    Given the definition (extend-protocol Countable Pair (count [self] 2))
    Given the definition (defun test () (let ((s (Single 1)) (p (Pair 2 3))) (+ (count s) (count p))))
    When I evaluate (test)
    Then the result is 3

  # MutVector tests

  Scenario: MutVector conj and nth via protocols
    Given the definition (defprotocol Collection (conj [self x]) (pop [self]))
    Given the definition (defprotocol Indexable (nth [self idx]))
    Given the definition (defstruct MutVector (data: ptr))
    Given the definition (defun mv-get-len (data: ptr) (aget data 0))
    Given the definition (defun mv-set-len! (data: ptr len) (aset data 0 len))
    Given the definition (defun mv-data-idx (idx) (+ idx 2))
    Given the definition (defun mut-vector () -> ptr (let ((cap 8) (data (heap-array (+ cap 2))) (s0 (aset data 0 0)) (s1 (aset data 1 cap))) (share (MutVector data))))
    Given the definition (extend-protocol Indexable MutVector (nth [self idx] (aget (. self data) (mv-data-idx idx))))
    Given the definition (extend-protocol Collection MutVector (conj [self x] (let ((data (. self data)) (len (mv-get-len data)) (s1 (aset data (mv-data-idx len) x)) (s2 (mv-set-len! data (+ len 1)))) self)) (pop [self] (let ((data (. self data)) (len (mv-get-len data))) (if (= len 0) 0 (let ((last-idx (- len 1)) (val (aget data (mv-data-idx last-idx))) (s (mv-set-len! data last-idx))) val)))))
    Given the definition (defun test () (let ((v (mut-vector))) (let ((v (conj v 10))) (let ((v (conj v 20))) (let ((v (conj v 30))) (+ (nth v 0) (+ (nth v 1) (nth v 2))))))))
    When I evaluate (test)
    Then the result is 60

  Scenario: MutVector count via Countable protocol
    Given the definition (defprotocol Countable (count [self]))
    Given the definition (defprotocol Collection (conj [self x]) (pop [self]))
    Given the definition (defstruct MutVector (data: ptr))
    Given the definition (defun mv-get-len (data: ptr) (aget data 0))
    Given the definition (defun mv-set-len! (data: ptr len) (aset data 0 len))
    Given the definition (defun mv-data-idx (idx) (+ idx 2))
    Given the definition (defun mut-vector () -> ptr (let ((cap 8) (data (heap-array (+ cap 2))) (s0 (aset data 0 0)) (s1 (aset data 1 cap))) (share (MutVector data))))
    Given the definition (extend-protocol Countable MutVector (count [self] (mv-get-len (. self data))))
    Given the definition (extend-protocol Collection MutVector (conj [self x] (let ((data (. self data)) (len (mv-get-len data)) (s1 (aset data (mv-data-idx len) x)) (s2 (mv-set-len! data (+ len 1)))) self)) (pop [self] (let ((data (. self data)) (len (mv-get-len data))) (if (= len 0) 0 (let ((last-idx (- len 1)) (val (aget data (mv-data-idx last-idx))) (s (mv-set-len! data last-idx))) val)))))
    Given the definition (defun test () (let ((v (mut-vector))) (let ((v (conj v 10))) (let ((v (conj v 20))) (count v)))))
    When I evaluate (test)
    Then the result is 2

  Scenario: MutVector pop returns last element
    Given the definition (defprotocol Collection (conj [self x]) (pop [self]))
    Given the definition (defstruct MutVector (data: ptr))
    Given the definition (defun mv-get-len (data: ptr) (aget data 0))
    Given the definition (defun mv-set-len! (data: ptr len) (aset data 0 len))
    Given the definition (defun mv-data-idx (idx) (+ idx 2))
    Given the definition (defun mut-vector () -> ptr (let ((cap 8) (data (heap-array (+ cap 2))) (s0 (aset data 0 0)) (s1 (aset data 1 cap))) (share (MutVector data))))
    Given the definition (extend-protocol Collection MutVector (conj [self x] (let ((data (. self data)) (len (mv-get-len data)) (s1 (aset data (mv-data-idx len) x)) (s2 (mv-set-len! data (+ len 1)))) self)) (pop [self] (let ((data (. self data)) (len (mv-get-len data))) (if (= len 0) 0 (let ((last-idx (- len 1)) (val (aget data (mv-data-idx last-idx))) (s (mv-set-len! data last-idx))) val)))))
    Given the definition (defun test () (let ((v (mut-vector))) (let ((v (conj v 100))) (let ((v (conj v 200))) (pop v)))))
    When I evaluate (test)
    Then the result is 200

  # Protocol default implementations

  Scenario: Default implementation via source protocol
    Given the definition (defprotocol Seq (first [self]) (rest [self]))
    Given the definition (defprotocol Headable (get-head [self]))
    Given the definition (defstruct Cons (head: i64 tail: ptr))
    Given the definition (extend-protocol Seq Cons (first [self] (. self head)) (rest [self] (. self tail)))
    Given the definition (extend-protocol-default Headable Seq (get-head [self] (first self)))
    Given the definition (defun test () (let ((xs (Cons 42 nil))) (get-head xs)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Direct impl takes precedence over default
    Given the definition (defprotocol Seq (first [self]) (rest [self]))
    Given the definition (defprotocol Countable (count [self]))
    Given the definition (defstruct FastList (len: i64 data: ptr))
    Given the definition (extend-protocol Seq FastList (first [self] 0) (rest [self] nil))
    Given the definition (extend-protocol-default Countable Seq (count [self] 999))
    Given the definition (extend-protocol Countable FastList (count [self] (. self len)))
    Given the definition (defun test () (let ((xs (FastList 42 nil))) (count xs)))
    When I evaluate (test)
    Then the result is 42

  Scenario: Default impl with multiple args
    Given the definition (defprotocol Seq (first [self]) (rest [self]))
    Given the definition (defprotocol Addable (add-n [self n]))
    Given the definition (defstruct Single (val: i64))
    Given the definition (extend-protocol Seq Single (first [self] (. self val)) (rest [self] nil))
    Given the definition (extend-protocol-default Addable Seq (add-n [self n] (+ (first self) n)))
    Given the definition (defun test () (let ((s (Single 10))) (add-n s 5)))
    When I evaluate (test)
    Then the result is 15

  # Generic protocols with type parameters

  Scenario: Generic protocol with type parameter
    Given the definition (defprotocol Getter<T> (get-val [self] -> T))
    Given the definition (defstruct IntBox (val: i64))
    Given the definition (extend-protocol Getter<i64> IntBox (get-val [self] (. self val)))
    Given the definition (defun test () (let ((box (IntBox 123))) (get-val box)))
    When I evaluate (test)
    Then the result is 123

  Scenario: Generic protocol static dispatch
    Given the definition (defprotocol Seq<T> (head [self] -> T) (tail [self] -> ptr))
    Given the definition (defstruct ICons (hd: i64 tl: ptr))
    Given the definition (extend-protocol Seq<i64> ICons (head [self] (. self hd)) (tail [self] (. self tl)))
    Given the definition (defun sum (xs: ptr) -> i64 (if (nil? xs) 0 (+ (head xs) (sum (tail xs)))))
    Given the definition (defun test () (sum (share (ICons 1 (share (ICons 2 (share (ICons 3 nil))))))))
    When I evaluate (test)
    Then the result is 6
