module Art.Toolbox exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Result
import Color exposing (..)
import ColorMath


view : (Color -> msg) -> (Float -> msg) -> Html msg
view colormsg sizemsg =
    div []
        [ div [] <| List.map (boxColor colormsg) colorList
        , slider sizemsg
        ]



-- color picker


colorList : List Color
colorList =
    [ white
    , grey , lightGrey , darkGrey
    , charcoal , lightCharcoal , darkCharcoal
    , red , lightRed , darkRed
    , orange , lightOrange , darkOrange
    , yellow , lightYellow , darkYellow
    , green , lightGreen , darkGreen
    , blue , lightBlue , darkBlue
    , purple , lightPurple , darkPurple
    , brown , lightBrown , darkBrown
    , black
    ]


boxColor : (Color -> msg) -> Color -> Html msg
boxColor colormsg color =
    let
        myStyle =
            style
                [ ( "background-color", "#" ++ ColorMath.colorToHex color )
                , ( "height", "20px" )
                , ( "width", "20px" )
                ]
    in
        Html.button [ myStyle, onClick (colormsg color) ] [ Html.text "  " ]



-- Size picker


slider : (Float -> msg) -> Html msg
slider callback =
    let
        resultString : String -> Float
        resultString jsonInput =
            Result.withDefault 10 <| Json.decodeString Json.float jsonInput
    in
        Html.input [ type_ "range", onInput (callback << resultString) ] []
