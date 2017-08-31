module Art.Canvas
    exposing
        ( Canvas
        , new
        , view
        , update
        , Msg
        )

import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Collage
import Element as GraphElement
import ElementRelativeMouseEvents as MouseE exposing (Point)
import List.Nonempty as NE exposing (Nonempty)
import Art.Stroke as Stroke exposing (Stroke)
import Art.Toolbox as Toolbox


type State
    = Drawing Stroke
    | Hovering Point


type Msg
    = Hover Point
    | Lift Point
    | Press Point
    | ChangeColor Color
    | ChangePenSize Float


type alias Canvas =
    { strokes : List Stroke
    , state : State
    , color : Color
    , strokeSize : Float
    , width : Float
    , height : Float
    }


{-| A default canvas
-}
new : Canvas
new =
    Canvas [] (Hovering <| Point 0 0) Color.black 20 600 400



-- VIEW


{-| Displays a stroke
-}
strokeToForm : Stroke -> Collage.Form
strokeToForm { points, color, size } =
    if NE.isSingleton points then
        Collage.circle (size / 2)
            |> Collage.filled color
            |> (Collage.move <| (\{ x, y } -> ( x, y )) <| NE.head points)
    else
        points
            |> NE.map (\{ x, y } -> ( x, y ))
            |> NE.toList
            |> Collage.path
            |> Collage.traced
                { color = color
                , width = size
                , cap = Collage.Round
                , join = Collage.Smooth
                , dashing = []
                , dashOffset = 0
                }


{-| Display the outline of the brush when howevering over the canvas
-}
brushOutline : Float -> Color -> ( Float, Float ) -> Collage.Form
brushOutline strokeSize color position =
    Collage.circle (strokeSize / 2)
        |> Collage.outlined
            { color = color
            , width = 1
            , cap = Collage.Flat
            , join = Collage.Smooth
            , dashing = List.repeat 2 <| ceiling <| strokeSize / 10
            , dashOffset = 0
            }
        |> Collage.move position


{-| Displays the whole drawing including previous strokes
-}
toForms : Canvas -> List Collage.Form
toForms { state, strokeSize, strokes, color } =
    let
        drawingTip =
            case state of
                Hovering { x, y } ->
                    [ brushOutline strokeSize color ( x, y ) ]

                Drawing stroke ->
                    [ strokeToForm stroke ]
    in
        strokes
            |> List.map strokeToForm
            |> (flip (++)) drawingTip


canvasView : Canvas -> Html Msg
canvasView ({ width, height } as canvas) =
    let
        canvasCoords : Point -> Point
        canvasCoords { x, y } =
            { x = x - width / 2, y = height / 2 - y }
    in
        toForms canvas
            |> Collage.collage (round width) (round height)
            |> GraphElement.toHtml
            |> List.singleton
            |> div
                [ MouseE.onMouseMove (canvasCoords >> Hover)
                , MouseE.onMouseUp (canvasCoords >> Lift)
                , MouseE.onMouseDown (canvasCoords >> Press)
                , id "drawingcontainer"
                ]


view : Canvas -> Html Msg
view canvas =
    div []
        [ canvasView canvas
        , div [ id "toolbox" ] [ Toolbox.view ChangeColor ChangePenSize ]
        ]



-- UPDATE


{-| Lift the "pen" from the canvas, that means the stroke is finished
-}
lift : Point -> Canvas -> Canvas
lift point canvas =
    case canvas.state of
        Drawing stroke ->
            { canvas
                | strokes = canvas.strokes ++ [ stroke ]
                , state = Hovering point
            }

        Hovering _ ->
            canvas


hover : Point -> Canvas -> Canvas
hover point ({ state, width, height } as canvas) =
    let
        newstate =
            case state of
                Drawing stroke ->
                    Drawing <| Stroke.draw point stroke

                Hovering _ ->
                    Hovering point
    in
        { canvas | state = newstate }


press : Point -> Canvas -> Canvas
press point ({ strokeSize, color, width, height } as canvas) =
    { canvas | state = Drawing <| Stroke.new point color strokeSize }


update : Msg -> Canvas -> Canvas
update msg canvas =
    case msg of
        Hover point ->
            hover point canvas

        Press point ->
            press point canvas

        Lift point ->
            lift point canvas

        ChangeColor newcolor ->
            { canvas | color = newcolor }

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize }
