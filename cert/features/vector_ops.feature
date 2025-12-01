Feature: Vector Operations (extractelement, insertelement, shufflevector)
  lIR supports LLVM's vector manipulation instructions.
  These allow element access and vector reshaping.

  Scenario: extractelement - Get element from vector
    Given the expression (extractelement (<4 x i32> 10 20 30 40) (i32 0))
    Then the result is (i32 10)

    Given the expression (extractelement (<4 x i32> 10 20 30 40) (i32 1))
    Then the result is (i32 20)

    Given the expression (extractelement (<4 x i32> 10 20 30 40) (i32 2))
    Then the result is (i32 30)

    Given the expression (extractelement (<4 x i32> 10 20 30 40) (i32 3))
    Then the result is (i32 40)

  Scenario: extractelement with float vectors
    Given the expression (extractelement (<2 x double> 3.14 2.71) (i32 0))
    Then the result is (double 3.14)

    Given the expression (extractelement (<2 x double> 3.14 2.71) (i32 1))
    Then the result is (double 2.71)

    Given the expression (extractelement (<4 x float> 1.0 2.0 3.0 4.0) (i32 2))
    Then the result is (float 3.0)

  Scenario: insertelement - Replace element in vector
    Given the expression (insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 0))
    Then the result is (<4 x i32> 99 2 3 4)

    Given the expression (insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 1))
    Then the result is (<4 x i32> 1 99 3 4)

    Given the expression (insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 2))
    Then the result is (<4 x i32> 1 2 99 4)

    Given the expression (insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 3))
    Then the result is (<4 x i32> 1 2 3 99)

  Scenario: insertelement with float vectors
    Given the expression (insertelement (<2 x double> 1.0 2.0) (double 9.9) (i32 0))
    Then the result is (<2 x double> 9.9 2.0)

    Given the expression (insertelement (<4 x float> 1.0 2.0 3.0 4.0) (float 0.0) (i32 2))
    Then the result is (<4 x float> 1.0 2.0 0.0 4.0)

  Scenario: shufflevector - Rearrange elements
    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 5 6 7 8) (<4 x i32> 0 1 2 3))
    Then the result is (<4 x i32> 1 2 3 4)

    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 5 6 7 8) (<4 x i32> 4 5 6 7))
    Then the result is (<4 x i32> 5 6 7 8)

    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 5 6 7 8) (<4 x i32> 0 4 1 5))
    Then the result is (<4 x i32> 1 5 2 6)

  Scenario: shufflevector - Reverse vector
    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 0 0 0 0) (<4 x i32> 3 2 1 0))
    Then the result is (<4 x i32> 4 3 2 1)

  Scenario: shufflevector - Broadcast element
    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 0 0 0 0) (<4 x i32> 0 0 0 0))
    Then the result is (<4 x i32> 1 1 1 1)

    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 0 0 0 0) (<4 x i32> 2 2 2 2))
    Then the result is (<4 x i32> 3 3 3 3)

  Scenario: shufflevector - Interleave
    Given the expression (shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 10 20 30 40) (<4 x i32> 0 4 1 5))
    Then the result is (<4 x i32> 1 10 2 20)

  Scenario: shufflevector with doubles
    Given the expression (shufflevector (<2 x double> 1.0 2.0) (<2 x double> 3.0 4.0) (<2 x i32> 0 2))
    Then the result is (<2 x double> 1.0 3.0)

    Given the expression (shufflevector (<2 x double> 1.0 2.0) (<2 x double> 3.0 4.0) (<2 x i32> 1 3))
    Then the result is (<2 x double> 2.0 4.0)

  Scenario: Chained vector operations
    Given the expression (extractelement (insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 0)) (i32 0))
    Then the result is (i32 99)

    Given the expression (insertelement (insertelement (<4 x i32> 0 0 0 0) (i32 1) (i32 0)) (i32 2) (i32 1))
    Then the result is (<4 x i32> 1 2 0 0)
