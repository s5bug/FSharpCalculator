namespace FSharpCalculator.Core.Tests

module CalculatorTests =
    open Expecto
    open FSharpCalculator.Core

    [<Tests>]
    let tests =
        testList
            "samples"
            [ test "1 + 2 tokenizes as [TNumber(1), TOp(+), TNumber(2)]" {
                Expect.equal
                    (Calculator.tokenize "1 + 2")
                    [| Calculator.TNumber 1.0
                       Calculator.TOp "+"
                       Calculator.TNumber 2.0 |]
                    "The token arrays should equal"
              }
              test "[TNumber(1), TOp(+), TNumber(2), TOp(*), TNumber(3)] parses as {1 2 3 * +}" {
                  Expect.equal
                      (Calculator.parse
                        [| Calculator.TNumber 1.0
                           Calculator.TOp "+"
                           Calculator.TNumber 2.0
                           Calculator.TOp "*"
                           Calculator.TNumber 3.0 |])
                      [ Calculator.Number 1.0
                        Calculator.Number 2.0
                        Calculator.Number 3.0
                        Calculator.Op "*"
                        Calculator.Op "+" ]
                      "The expression lists should equal"
              }
              test "{1 2 3 * +} should evaluate to 7" {
                  Expect.equal
                    (Calculator.evaluate
                        [ Calculator.Number 1.0
                          Calculator.Number 2.0
                          Calculator.Number 3.0
                          Calculator.Op "*"
                          Calculator.Op "+" ])
                    (Some 7.0)
                    "The result values should equal"
              } ]
