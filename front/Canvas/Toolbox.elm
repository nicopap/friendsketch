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
    | ChooseColor Int Color


type alias Toolbox =
    { palette : Array ColorBox
    , selected : Maybe Int
    }


{-| A color box is a cell in the canvas palette.
-}
type ColorBox
    = ColorBox Box Bool

type Box
    = Fixed Color
    | Modifiable (Maybe Color)


new : Toolbox
new =
    { palette =
        Array.append
            (Array.map (\col -> ColorBox (Fixed col) False) colorList)
            (Array.repeat 10 <| ColorBox (Modifiable Nothing) False)
    , selected = Nothing
    }


view : (Float -> msg) -> Toolbox -> Html (Result Msg msg)
view sizemsg { palette } =
    div [ id "toolbox" ]
        [ viewPalette palette
        , Html.map Ok <| slider sizemsg
        ]


colorList : Array Color
colorList =
    Array.fromList
        [ white, lightGrey, grey, darkGrey,  lightCharcoal, charcoal, darkCharcoal, lightRed, red, darkRed, lightOrange, orange, darkOrange, lightYellow, yellow, darkYellow, lightGreen, green, darkGreen, lightBlue, blue, darkBlue, lightPurple, purple, darkPurple, lightBrown, brown, darkBrown, black ]


viewPalette : Array ColorBox -> Html (Result Msg msg)
viewPalette boxes =
    div [ id "palette" ] (Array.toList <| Array.indexedMap viewColorBox boxes)


withBackground : Color -> Html.Attribute msg
withBackground color =
    style [ ( "background-color", "#" ++ colorToHex color ) ]


viewColorBox : Int -> ColorBox -> Html (Result Msg msg)
viewColorBox index (ColorBox box selected) =
    case box of
        Fixed color  ->
            button
                [ withBackground color
                , selectableClass selected "colorbox"
                , onClick (Err <| ChooseColor index <| color)
                ]
                [ Html.text "" ]

        Modifiable (Just color) ->
            input
                [ type_ "color"
                , selectableClass selected "custom colorbox"
                , withBackground color
                , onInput (Err << SetColor index << Result.withDefault grey << hexToColor)
                ]
                []

        Modifiable Nothing ->
            input
                [ type_ "color"
                , class "empty colorbox"
                , onInput (Err << SetColor index << Result.withDefault grey << hexToColor)
                ]
                []


selectableClass : Bool -> String -> Html.Attribute msg
selectableClass selected class_ =
    if selected then
        class (class_ ++ " selected")
    else
        class class_


update : Msg -> Toolbox -> ( Toolbox, Color )
update msg toolbox =
    let
        set f color =
            ColorBox (f color) True

        palette : Array ColorBox
        palette =
            toolbox.selected
                |> Maybe.andThen (\i ->
                    Maybe.map (\x->(i,x)) (Array.get i toolbox.palette))
                |> Maybe.map (\(i, ColorBox box _) ->
                    Array.set i (ColorBox box False) toolbox.palette)
                |> Maybe.withDefault toolbox.palette
    in
        case msg of
            ChooseColor index color ->
                ( { palette = Array.set index (set Fixed color) palette
                  , selected = Just index
                  }
                , color
                )

            SetColor index color ->
                ( { palette = Array.set index (set Modifiable (Just color)) palette
                  , selected = Just index
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
