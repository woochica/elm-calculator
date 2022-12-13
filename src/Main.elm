module Main exposing (..)

import Browser
import Calculator exposing (Infix(..), Input(..), Number, infixToRpn, toResult)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Operator exposing (Operator(..))


type alias Model =
    { screen : String
    , input : List Input
    }


type Msg
    = GotNumber Number
    | GotOperator Operator
    | CalcResult
    | ClearScreen

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { screen = initScreen
    , input = []
    }


initScreen : String
initScreen =
    "0"


addToScreen : String -> Number -> String
addToScreen screen num =
    if screen == initScreen then
        overwriteScreen num

    else
        screen ++ String.fromFloat num

overwriteScreen : Number -> String
overwriteScreen num =
    String.fromFloat num


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotNumber num ->
            updateNumber num model

        GotOperator op ->
            updateOperator op model

        CalcResult ->
            calcResult model

        ClearScreen ->
            init

calcResult : Model -> Model
calcResult model =
            let
                result =
                    model.input
                        |> List.reverse
                        |> Infix
                        |> infixToRpn
                        |> toResult
            in
            case result of
                Ok num ->
                    { model | screen = overwriteScreen num }

                Err err ->
                    let
                        _ =
                            Debug.log "An error occured" err
                    in
                    model

updateOperator : Operator -> Model -> Model
updateOperator op model =
            case model.input of
                [] ->
                    model

                (Num a) :: rest ->
                    { model | input = Op op :: Num a :: rest }

                (Op _) :: memory ->
                    { model | input = Op op :: memory }

updateNumber : Number -> Model -> Model
updateNumber num model =
            let
                newScreen =
                    addToScreen model.screen num

                maybeNum =
                    String.toFloat newScreen
            in
            case (model.input, maybeNum) of
                ([], Just _) ->
                            { model
                                | screen = newScreen
                                , input = [ Num num ]
                            }
                ([], Nothing) ->
                    model


                ((Num _) :: rest, Just num_) ->
                            { model
                                | screen = newScreen
                                , input = Num num_ :: rest
                            }
                ((Num _) :: _, Nothing) ->

                            model

                ((Op op) :: rest, _) ->
                    { model
                        | screen = overwriteScreen num
                        , input = Num num :: Op op :: rest
                    }


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [ class "calculator__output" ] [ text model.screen ]
        , div [ class "calculator__keys" ]
            [ button [ class "calculator__key calculator__key--operator", onClick (GotOperator Add) ] [ text "+" ]
            , button [ class "calculator__key calculator__key--operator", onClick (GotOperator Sub) ] [ text "-" ]
            , button [ class "calculator__key calculator__key--operator", onClick (GotOperator Mul) ] [ text "×" ]
            , button [ class "calculator__key calculator__key--operator", onClick (GotOperator Div) ] [ text "÷" ]
            , button [ class "calculator__key", onClick (GotNumber 7) ] [ text "7" ]
            , button [ class "calculator__key", onClick (GotNumber 8) ] [ text "8" ]
            , button [ class "calculator__key", onClick (GotNumber 9) ] [ text "9" ]
            , button [ class "calculator__key", onClick (GotNumber 4) ] [ text "4" ]
            , button [ class "calculator__key", onClick (GotNumber 5) ] [ text "5" ]
            , button [ class "calculator__key", onClick (GotNumber 6) ] [ text "6" ]
            , button [ class "calculator__key", onClick (GotNumber 1) ] [ text "1" ]
            , button [ class "calculator__key", onClick (GotNumber 2) ] [ text "2" ]
            , button [ class "calculator__key", onClick (GotNumber 3) ] [ text "3" ]
            , button [ class "calculator__key", onClick (GotNumber 0) ] [ text "0" ]
            , button [ class "calculator__key", disabled True ] [ text "." ] -- Unsupported yet
            , button [ class "calculator__key", onClick ClearScreen ] [ text "AC" ]
            , button
                [ class "calculator__key  calculator__key--operator calculator__key--enter"
                , onClick CalcResult
                ]
                [ text "=" ]
            ]
        ]
