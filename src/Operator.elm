module Operator exposing (Operator(..)
                         , compare,
                         call, toString)

type Operator
    = Add
    | Sub
    | Mul
    | Div

compare : Operator -> Operator -> Order
compare op1 op2 =
    case (op1, op2) of
        (Add, Add) ->
            EQ
        (Add, Sub) ->
            EQ
        (Add, Mul) ->
            LT
        (Add, Div) ->
            LT
        (Sub, Add) ->
            EQ
        (Sub, Sub) ->
            EQ
        (Sub, Mul) ->
            LT
        (Sub, Div) ->
            LT
        (Mul, Add) ->
            GT
        (Mul, Sub) ->
            GT
        (Mul, Mul) ->
            EQ
        (Mul, Div) ->
            EQ
        (Div, Add) ->
            GT
        (Div, Sub) ->
            GT
        (Div, Mul) ->
            EQ
        (Div, Div) ->
            EQ



call : Operator -> (Float -> Float -> Float)
call op =
    case op of
        Add ->
            (+)

        Sub ->
            (-)

        Mul ->
            (*)

        Div ->
            (/)
toString : Operator -> String
toString op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"
