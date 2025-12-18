Feature: Closure Type Inference

  Scenario: Closure returning ptr used in nil comparison
    Given the definition (defun apply-or-nil (f x: ptr) -> ptr (let ((result (f x))) (if (nil? result) nil result)))
    Given the definition (defstruct Box (v: i64))
    Given the definition (defun test () -> i64 (let ((b (share (Box 42)))) (if (nil? (apply-or-nil (fn (x) x) b)) 0 42)))
    When I evaluate (test)
    Then the result is 42
