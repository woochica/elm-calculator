module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { screen : String
    , memory : Memory
    }


type Memory
    = Num Int
    | NumOp Int Operator


type alias Operator =
    Int -> Int -> Int


init : Model
init =
    { screen = "0"
    , memory = Num 0
    }


type Msg
    = NumberClicked Int
    | OperatorClicked Operator
    | ResultClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumberClicked number ->
            case model.memory of
                Num num ->
                    if model.screen == "0" then
                        { model
                            | screen = String.fromInt number
                        }

                    else
                        { model
                            | screen = model.screen ++ String.fromInt number
                        }

                NumOp num op ->
                    let
                        arg1 =
                            String.toInt model.screen |> Maybe.withDefault 0
                    in
                    if num == arg1 then
                        { model | screen = String.fromInt number }

                    else
                        { model | screen = model.screen ++ String.fromInt number }

        OperatorClicked operator ->
            let
                arg1 =
                    String.toInt model.screen
            in
            case arg1 of
                Just num ->
                    { model | memory = NumOp num operator }

                Nothing ->
                    model

        ResultClicked ->
            let
                arg2 =
                    String.toInt model.screen |> Maybe.withDefault 0
            in
            case model.memory of
                NumOp num op ->
                    let
                        result =
                            op num arg2
                    in
                    { model
                        | memory = Num arg2
                        , screen = String.fromInt result
                    }

                Num _ ->
                    model


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [ class "calculator__output" ] [ text model.screen ]
        , div [ class "calculator__keys" ]
            [ button
                [ class "calculator__key calculator__key--operator"
                , onClick (OperatorClicked (+))
                ]
                [ text "+" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "-" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "×" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "÷" ]
            , button [ class "calculator__key", onClick (NumberClicked 7) ] [ text "7" ]
            , button [ class "calculator__key", onClick (NumberClicked 8) ] [ text "8" ]
            , button [ class "calculator__key", onClick (NumberClicked 9) ] [ text "9" ]
            , button [ class "calculator__key", onClick (NumberClicked 4) ] [ text "4" ]
            , button [ class "calculator__key", onClick (NumberClicked 5) ] [ text "5" ]
            , button [ class "calculator__key", onClick (NumberClicked 6) ] [ text "6" ]
            , button [ class "calculator__key", onClick (NumberClicked 1) ] [ text "1" ]
            , button [ class "calculator__key", onClick (NumberClicked 2) ] [ text "2" ]
            , button [ class "calculator__key", onClick (NumberClicked 3) ] [ text "3" ]
            , button [ class "calculator__key", onClick (NumberClicked 0) ] [ text "0" ]
            , button [ class "calculator__key" ] [ text "." ]
            , button [ class "calculator__key" ] [ text "AC" ]
            , button
                [ class "calculator__key  calculator__key--operator calculator__key--enter"
                , onClick ResultClicked
                ]
                [ text "=" ]
            ]
        ]
