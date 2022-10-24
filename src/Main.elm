module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [ class "calculator__output" ] [ text "0" ]
        , div [ class "calculator__keys" ]
            [ button [ class "calculator__key calculator__key--operator" ] [ text "+" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "-" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "×" ]
            , button [ class "calculator__key calculator__key--operator" ] [ text "÷" ]
            , button [ class "calculator__key" ] [ text "7" ]
            , button [ class "calculator__key" ] [ text "8" ]
            , button [ class "calculator__key" ] [ text "9" ]
            , button [ class "calculator__key" ] [ text "4" ]
            , button [ class "calculator__key" ] [ text "5" ]
            , button [ class "calculator__key" ] [ text "6" ]
            , button [ class "calculator__key" ] [ text "1" ]
            , button [ class "calculator__key" ] [ text "2" ]
            , button [ class "calculator__key" ] [ text "3" ]
            , button [ class "calculator__key" ] [ text "0" ]
            , button [ class "calculator__key" ] [ text "." ]
            , button [ class "calculator__key" ] [ text "AC" ]
            , button [ class "calculator__key  calculator__key--operator calculator__key--enter" ] [ text "=" ]
            ]
        ]
