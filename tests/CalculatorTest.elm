module CalculatorTest exposing (suite)

import Calculator exposing (..)
import Expect
import Operator exposing (..)
import Test exposing (Test, describe, test)


testInfixToRpn : Test
testInfixToRpn =
    test "Infix to Rpn" <|
        \_ ->
            let
                expected : List ( Infix, Rpn )
                expected =
                    [ ( Infix [ Num 1, Op Add, Num 2 ], Rpn [ Num 1, Num 2, Op Add ] )
                    , ( Infix [ Num 1, Op Add, Num 2, Op Sub, Num 5, Op Add, Num 2, Op Mul, Num 4 ]
                      , Rpn [ Num 1, Num 2, Op Add, Num 5, Op Sub, Num 2, Num 4, Op Mul, Op Add ]
                      )
                    , ( Infix [ Num 1, Op Add, Num 2, Op Mul, Num 6, Op Div, Num 4 ]
                      , Rpn [ Num 1, Num 2, Num 6, Op Mul, Num 4, Op Div, Op Add ]
                      )
                    ]

                actual : List ( Infix, Rpn )
                actual =
                    expected
                        |> List.map
                            (\( infix, _ ) ->
                                ( infix, infixToRpn infix )
                            )
            in
            expected
                |> Expect.equal actual


testToResult : Test
testToResult =
    test "Calculate result" <|
        \_ ->
            let
                expected =
                    [ ( Infix [ Num 1, Op Add, Num 2 ], Ok 3 )
                    , ( Infix [ Num 1, Op Add, Num 2, Op Mul, Num 3 ], Ok 7 )
                    , ( Infix [ Num 1, Op Add, Num 2, Op Mul, Num 6, Op Div, Num 4 ], Ok 4 )
                    ]

                actual =
                    expected
                        |> List.map
                            (\( memory, _ ) ->
                                ( memory, memory |> infixToRpn |> toResult )
                            )
            in
            expected
                |> Expect.equal actual


suite : Test
suite =
    describe "Calculator tests"
        [ testInfixToRpn
        , testToResult
        ]
