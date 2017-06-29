module Art.Tools.ColorPicker exposing (newTool, State, Msg)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Color exposing (..)
import ColorMath
import Art.Canvas as Canvas


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


type Msg
    = ChangeColor Color


type alias State =
    { color : Color
    }


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg state =
    case msg of
        ChangeColor newcolor ->
            ( { state | color = newcolor }
            , Canvas.changeColor newcolor
            )


boxColor : Color -> Html Msg
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


view : State -> Html Msg
view _ =
    Html.div []
        (List.map boxColor colorList)


newTool : Canvas.Tool State Msg
newTool =
    { state = { color = black }
    , update = update
    , view = view
    }
