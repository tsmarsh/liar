Feature: Struct Update via assoc Macro

  The assoc macro provides functional struct updates using compile-time reflection.
  It creates a new struct with one field changed, preserving other fields.

  Background:
    Given the definition (defmacro assoc (struct-type instance field new-value) (let ((fields (struct-fields struct-type)) (field-exprs (map (fn (f) (if (= field f) new-value (make-field-access instance f))) fields))) `(,struct-type ,@field-exprs)))

  Scenario: Update first field of two-field struct
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (. (assoc Point p x 10) x)))
    When I evaluate (test)
    Then the result is 10

  Scenario: Update second field of two-field struct
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (. (assoc Point p y 20) y)))
    When I evaluate (test)
    Then the result is 20

  Scenario: Other fields are preserved
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (. (assoc Point p x 10) y)))
    When I evaluate (test)
    Then the result is 2

  Scenario: Original struct is unchanged (immutable update)
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (let ((p2 (assoc Point p x 10))) (. p x))))
    When I evaluate (test)
    Then the result is 1

  Scenario: Update middle field of three-field struct
    Given the definition (defstruct Vec3 (x: i64 y: i64 z: i64))
    Given the definition (defun test () (let ((v (Vec3 1 2 3))) (. (assoc Vec3 v y 20) y)))
    When I evaluate (test)
    Then the result is 20

  Scenario: Update last field of three-field struct
    Given the definition (defstruct Vec3 (x: i64 y: i64 z: i64))
    Given the definition (defun test () (let ((v (Vec3 1 2 3))) (. (assoc Vec3 v z 30) z)))
    When I evaluate (test)
    Then the result is 30

  Scenario: Chain multiple assoc calls
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (let ((p2 (assoc Point p x 10))) (let ((p3 (assoc Point p2 y 20))) (+ (. p3 x) (. p3 y))))))
    When I evaluate (test)
    Then the result is 30

  Scenario: Use assoc with computed value
    Given the definition (defstruct Point (x: i64 y: i64))
    Given the definition (defun test () (let ((p (Point 1 2))) (. (assoc Point p x (+ (. p x) 9)) x)))
    When I evaluate (test)
    Then the result is 10
