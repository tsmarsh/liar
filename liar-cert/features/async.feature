Feature: Async Primitives

  The Pollable protocol and async futures provide the foundation
  for non-blocking asynchronous programming in liar.

  Scenario: ImmediateFuture returns value immediately
    Given the definition (defprotocol Pollable (poll [self waker]))
    Given the definition (defstruct ImmediateFuture (value: i64))
    Given the definition (extend-protocol Pollable ImmediateFuture (poll [self waker] (. self value)))
    Given the definition (defun test () (let ((f (ImmediateFuture 42))) (poll f 0)))
    When I evaluate (test)
    Then the result is 42

  Scenario: NeverFuture returns pending (0)
    Given the definition (defprotocol Pollable (poll [self waker]))
    Given the definition (defstruct NeverFuture (dummy: i64))
    Given the definition (extend-protocol Pollable NeverFuture (poll [self waker] 0))
    Given the definition (defun test () (let ((f (NeverFuture 0))) (poll f 0)))
    When I evaluate (test)
    Then the result is 0

  Scenario: Multiple types implement Pollable
    Given the definition (defprotocol Pollable (poll [self waker]))
    Given the definition (defstruct ReadyFuture (val: i64))
    Given the definition (extend-protocol Pollable ReadyFuture (poll [self waker] (. self val)))
    Given the definition (defun test () (let ((f (ReadyFuture 100))) (poll f 0)))
    When I evaluate (test)
    Then the result is 100

  Scenario: Future with computed value
    Given the definition (defprotocol Pollable (poll [self waker]))
    Given the definition (defstruct ComputeFuture (a: i64 b: i64))
    Given the definition (extend-protocol Pollable ComputeFuture (poll [self waker] (+ (. self a) (. self b))))
    Given the definition (defun test () (let ((f (ComputeFuture 10 32))) (poll f 0)))
    When I evaluate (test)
    Then the result is 42
