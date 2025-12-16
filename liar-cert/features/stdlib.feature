Feature: Standard Library Functions

  Core data transformation and sequence operations

  Scenario: Thread-first with bare functions
    Given the definition (defmacro thread-first (x ... forms) (if (nil? forms) x (let ((form (first forms)) (more (rest forms))) (if (nil? more) (if (list? form) `(,(first form) ,x ,@(rest form)) `(,form ,x)) (if (list? form) `(thread-first (,(first form) ,x ,@(rest form)) ,@more) `(thread-first (,form ,x) ,@more))))))
    Given the definition (defun inc2 (x) (+ x 1))
    Given the definition (defun test () (thread-first 5 inc2 inc2))
    When I evaluate (test)
    Then the result is 7

  Scenario: Thread-first with function calls
    Given the definition (defmacro thread-first (x ... forms) (if (nil? forms) x (let ((form (first forms)) (more (rest forms))) (if (nil? more) (if (list? form) `(,(first form) ,x ,@(rest form)) `(,form ,x)) (if (list? form) `(thread-first (,(first form) ,x ,@(rest form)) ,@more) `(thread-first (,form ,x) ,@more))))))
    Given the definition (defun add (a b) (+ a b))
    Given the definition (defun test () (thread-first 1 (add 2) (add 3)))
    When I evaluate (test)
    Then the result is 6

  Scenario: Thread-first mixed forms
    Given the definition (defmacro thread-first (x ... forms) (if (nil? forms) x (let ((form (first forms)) (more (rest forms))) (if (nil? more) (if (list? form) `(,(first form) ,x ,@(rest form)) `(,form ,x)) (if (list? form) `(thread-first (,(first form) ,x ,@(rest form)) ,@more) `(thread-first (,form ,x) ,@more))))))
    Given the definition (defun inc2 (x) (+ x 1))
    Given the definition (defun mul (a b) (* a b))
    Given the definition (defun test () (thread-first 2 inc2 (mul 3)))
    When I evaluate (test)
    Then the result is 9

  Scenario: Thread-last with bare functions
    Given the definition (defmacro thread-last (x ... forms) (if (nil? forms) x (let ((form (first forms)) (more (rest forms))) (if (nil? more) (if (list? form) `(,@form ,x) `(,form ,x)) (if (list? form) `(thread-last (,@form ,x) ,@more) `(thread-last (,form ,x) ,@more))))))
    Given the definition (defun inc2 (x) (+ x 1))
    Given the definition (defun dec2 (x) (- x 1))
    Given the definition (defun test () (thread-last 5 inc2 dec2))
    When I evaluate (test)
    Then the result is 5

  Scenario: Range generates sequence
    Given the definition (defstruct Cons (head: i64 tail: ptr))
    Given the definition (defun cons (head tail: ptr) -> ptr (share (Cons head tail)))
    Given the definition (defun first (seq: ptr) (. seq head))
    Given the definition (defun rest (seq: ptr) -> ptr (. seq tail))
    Given the definition (defun range (start end) -> ptr (if (>= start end) nil (cons start (range (+ start 1) end))))
    Given the definition (defun sum (seq: ptr) (if (nil? seq) 0 (+ (first seq) (sum (rest seq)))))
    Given the definition (defun test () (sum (range 0 5)))
    When I evaluate (test)
    Then the result is 10

  Scenario: Repeat-n creates copies
    Given the definition (defstruct Cons (head: i64 tail: ptr))
    Given the definition (defun cons (head tail: ptr) -> ptr (share (Cons head tail)))
    Given the definition (defun first (seq: ptr) (. seq head))
    Given the definition (defun rest (seq: ptr) -> ptr (. seq tail))
    Given the definition (defun repeat-n (n x) -> ptr (if (<= n 0) nil (cons x (repeat-n (- n 1) x))))
    Given the definition (defun sum (seq: ptr) (if (nil? seq) 0 (+ (first seq) (sum (rest seq)))))
    Given the definition (defun test () (sum (repeat-n 4 5)))
    When I evaluate (test)
    Then the result is 20

  Scenario: Partial1 fixes first argument
    Given the definition (defun partial1 (f a) (fn (x) (f a x)))
    Given the definition (defun add (a b) (+ a b))
    Given the definition (defun test () ((partial1 add 10) 5))
    When I evaluate (test)
    Then the result is 15

  Scenario: Pipe composes left to right
    Given the definition (defun pipe (f g) (fn (x) (g (f x))))
    Given the definition (defun inc2 (x) (+ x 1))
    Given the definition (defun square (x) (* x x))
    Given the definition (defun test () ((pipe inc2 square) 3))
    When I evaluate (test)
    Then the result is 16
