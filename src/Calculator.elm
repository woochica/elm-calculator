module Calculator exposing (Infix(..), Input(..), Number, Rpn(..), infixToRpn, toOperator, toResult)

import List.Extra
import Operator exposing (Operator(..))


type Infix
    = Infix (List Input)


type Rpn
    = Rpn (List Input)


type Input
    = Op Operator
    | Num Number


type alias Number =
    Float


toOperator : Input -> Maybe Operator
toOperator item =
    case item of
        Op Add ->
            Just Add

        Op Sub ->
            Just Sub

        Op Mul ->
            Just Mul

        Op Div ->
            Just Div

        Num _ ->
            Nothing


isOperator : Input -> Bool
isOperator item =
    case item of
        Op _ ->
            True

        Num _ ->
            False


{-| Convert input list from infix notation to RPN (Reverse Polish Notation).
-}
infixToRpn : Infix -> Rpn
infixToRpn (Infix infix) =
    Rpn (infixToRpn_ [] [] infix)


infixToRpn_ : List Input -> List Operator -> List Input -> List Input
infixToRpn_ output operators input =
    case input of
        item :: rest ->
            let
                ( output2, operators2 ) =
                    convertInput item output operators
            in
            infixToRpn_ output2 operators2 rest

        [] ->
            output ++ List.map Op operators


{-| Convert the input item.

Uses the Shunting-yard algorithm.

There are 2 rules:

  - literals always goes to the output stack

  - when an operator with lower or equal precedence than the last element of the
    operators stack is comming, pop the last element of the operators stack to the
    output stack

Source: <https://dev.to/quantumsheep/how-calculators-read-mathematical-expression-with-operator-precedence-4n9h>

-}
convertInput : Input -> List Input -> List Operator -> ( List Input, List Operator )
convertInput item output operators =
    case item of
        Num num ->
            ( output ++ [ Num num ]
            , operators
            )

        Op op ->
            case operators of
                firstOp :: restOps ->
                    if Operator.compare op firstOp == GT then
                        ( output
                        , op :: operators
                        )

                    else
                        ( output ++ [ Op firstOp ]
                        , op :: restOps
                        )

                [] ->
                    ( output
                    , [ op ]
                    )


toResult : Rpn -> Result String Number
toResult (Rpn memory) =
    toResult_ memory


toResult_ : List Input -> Result String Number
toResult_ memory =
    case calcNextOperation memory of
        Ok [ Num num ] ->
            Ok num

        Ok newMemory ->
            toResult_ newMemory

        Err err ->
            Err err


calcNextOperation : List Input -> Result String (List Input)
calcNextOperation memory =
    let
        ( left, right ) =
            List.Extra.break isOperator memory
    in
    case ( List.reverse left, right ) of
        ( (Num b) :: (Num a) :: restReversedLeft, (Op op) :: restRight ) ->
            let
                result =
                    Operator.call op a b |> Num

                newMemory =
                    List.reverse restReversedLeft ++ result :: restRight
            in
            Ok newMemory

        _ ->
            Err <| "invalid input"
