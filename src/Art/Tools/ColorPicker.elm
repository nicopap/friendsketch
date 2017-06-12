module Art.Tools.ColorPicker exposing (view)

import Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Color exposing (..)
import Color
import ColorMath
import Art.ToolboxMsg exposing (ToolboxMsg(..))


colorList =
    [ white
    , grey
    , lightGrey
    , darkGrey
    , charcoal
    , lightCharcoal
    , darkCharcoal
    , red
    , lightRed
    , darkRed
    , orange
    , lightOrange
    , darkOrange
    , yellow
    , lightYellow
    , darkYellow
    , green
    , lightGreen
    , darkGreen
    , blue
    , lightBlue
    , darkBlue
    , purple
    , lightPurple
    , darkPurple
    , brown
    , lightBrown
    , darkBrown
    , black
    ]


boxColor : Color -> Html.Html ToolboxMsg
boxColor color =
    let
        myStyle =
            style
                [ ( "background-color", "#" ++ ColorMath.colorToHex color )
                , ( "height", "20px" )
                , ( "width", "20px" )
                ]
    in
        Html.button [ myStyle, onClick (ChangeColor color) ] [ Html.text "  " ]


view : Html.Html ToolboxMsg
view =
    Html.div []
        (List.map boxColor colorList)
