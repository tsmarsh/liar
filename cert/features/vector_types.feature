Feature: Vector Types
  lIR supports LLVM's vector types for SIMD operations.
  Vectors are fixed-size collections of primitive types.

  Scenario: Integer vector literals
    Given the expression (<4 x i32> 1 2 3 4)
    Then the result is (<4 x i32> 1 2 3 4)

    Given the expression (<2 x i64> 100 200)
    Then the result is (<2 x i64> 100 200)

    Given the expression (<8 x i8> 1 2 3 4 5 6 7 8)
    Then the result is (<8 x i8> 1 2 3 4 5 6 7 8)

    Given the expression (<16 x i8> 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    Then the result is (<16 x i8> 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

  Scenario: Float vector literals
    Given the expression (<4 x float> 1.0 2.0 3.0 4.0)
    Then the result is (<4 x float> 1.0 2.0 3.0 4.0)

    Given the expression (<2 x double> 3.14 2.71)
    Then the result is (<2 x double> 3.14 2.71)

  Scenario: Negative values in vectors
    Given the expression (<4 x i32> -1 -2 -3 -4)
    Then the result is (<4 x i32> -1 -2 -3 -4)

    Given the expression (<2 x double> -1.5 -2.5)
    Then the result is (<2 x double> -1.5 -2.5)

  Scenario: i1 vectors (boolean vectors)
    Given the expression (<4 x i1> 1 0 1 0)
    Then the result is (<4 x i1> 1 0 1 0)

    Given the expression (<8 x i1> 1 1 0 0 1 1 0 0)
    Then the result is (<8 x i1> 1 1 0 0 1 1 0 0)

  Scenario: Common SIMD vector sizes
    Given the expression (<4 x i32> 1 2 3 4)
    Then the result is (<4 x i32> 1 2 3 4)

    Given the expression (<4 x float> 1.0 2.0 3.0 4.0)
    Then the result is (<4 x float> 1.0 2.0 3.0 4.0)

    Given the expression (<2 x double> 1.0 2.0)
    Then the result is (<2 x double> 1.0 2.0)

    Given the expression (<8 x i16> 1 2 3 4 5 6 7 8)
    Then the result is (<8 x i16> 1 2 3 4 5 6 7 8)

  Scenario: Vector arithmetic (element-wise)
    Given the expression (add (<4 x i32> 1 2 3 4) (<4 x i32> 10 20 30 40))
    Then the result is (<4 x i32> 11 22 33 44)

    Given the expression (fadd (<2 x double> 1.0 2.0) (<2 x double> 0.5 0.5))
    Then the result is (<2 x double> 1.5 2.5)

    Given the expression (mul (<4 x i32> 2 3 4 5) (<4 x i32> 10 10 10 10))
    Then the result is (<4 x i32> 20 30 40 50)

  Scenario: Vector comparison (element-wise)
    Given the expression (icmp eq (<4 x i32> 1 2 3 4) (<4 x i32> 1 0 3 0))
    Then the result is (<4 x i1> 1 0 1 0)

    Given the expression (fcmp olt (<2 x double> 1.0 2.0) (<2 x double> 1.5 1.5))
    Then the result is (<2 x i1> 1 0)

  Scenario: Vector splat (all same value)
    Given the expression (<4 x i32> 42 42 42 42)
    Then the result is (<4 x i32> 42 42 42 42)

    Given the expression (<8 x i8> 0 0 0 0 0 0 0 0)
    Then the result is (<8 x i8> 0 0 0 0 0 0 0 0)
