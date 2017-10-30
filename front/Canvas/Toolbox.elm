module Canvas.Toolbox exposing (new, view, update, Msg, Toolbox)

import Html exposing (Html, div, button, p, input)
import Html.Attributes exposing (style, type_, class, id)
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Json.Decode as Json
import Result
import Array exposing (Array)
import Color exposing (..)
import ColorMath exposing (colorToHex, hexToColor)


type Msg
    = SetColor Int Color


type alias Toolbox =
    { palette : Array ColorBox
    }


{-| A color box is a cell in the canvas palette.
-}
type ColorBox
    = Fixed Color
    | Modifiable (Maybe Color)


new : Toolbox
new =
    { palette =
        Array.append
            (Array.map Fixed colorList)
            (Array.repeat 10 <| Modifiable Nothing)
    }


view : (Color -> msg) -> (Float -> msg) -> Toolbox -> Html (Result Msg msg)
view colormsg sizemsg { palette } =
    div [ id "toolbox" ]
        [ viewPalette colormsg palette
        , Html.map Ok <| slider sizemsg
        ]


colorList : Array Color
colorList =
    Array.fromList
        [ white, grey, lightGrey, darkGrey, charcoal, lightCharcoal, darkCharcoal, red, lightRed, darkRed, orange, lightOrange, darkOrange, yellow, lightYellow, darkYellow, green, lightGreen, darkGreen, blue, lightBlue, darkBlue, purple, lightPurple, darkPurple, brown, lightBrown, darkBrown, black ]


viewPalette : (Color -> msg) -> Array ColorBox -> Html (Result Msg msg)
viewPalette colormsg boxes =
    div [ id "palette" ] <|
        Array.toList <|
            Array.indexedMap (viewColorBox colormsg) boxes


withBackground : Color -> Html.Attribute msg
withBackground color =
    style [ ( "background-color", "#" ++ colorToHex color ) ]


viewColorBox : (Color -> msg) -> Int -> ColorBox -> Html (Result Msg msg)
viewColorBox colormsg index box =
    case box of
        Fixed color ->
            button
                [ withBackground color
                , class "colorbox"
                , onClick ((Ok << colormsg) color)
                ]
                [ Html.text "  " ]

        Modifiable (Just color) ->
            input
                [ type_ "color"
                , class "customcolorbox colorbox"
                , withBackground color
                , onInput (Err << SetColor index << Result.withDefault grey << hexToColor)
                ]
                []

        Modifiable Nothing ->
            input
                [ type_ "color"
                , class "emptycolorbox colorbox"
                , onInput (Err << SetColor index << Result.withDefault grey << hexToColor)
                ]
                []


update : Msg -> Toolbox -> ( Toolbox, Color )
update msg ({ palette } as toolbox) =
    case msg of
        SetColor index color ->
            ( { toolbox
                | palette = Array.set index (Modifiable (Just color)) palette
              }
            , color
            )



-- Size picker


slider : (Float -> msg) -> Html msg
slider continuation =
    let
        resultString : String -> Float
        resultString jsonInput =
            Result.withDefault 10 <| Json.decodeString Json.float jsonInput
    in
        input [ type_ "range", onInput (continuation << resultString) ] []
