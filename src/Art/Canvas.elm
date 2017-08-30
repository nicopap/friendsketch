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
    = Drawing (Maybe Stroke)
    | Selecting
    | Hovering Point


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
    Canvas [] Selecting Color.black 20 600 400



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
            , dashing = [ 1, round <| strokeSize / 10 ]
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

                Drawing (Just stroke) ->
                    [ strokeToForm stroke ]

                Drawing Nothing ->
                    []

                Selecting ->
                    []
    in
        strokes
            |> List.map strokeToForm
            |> (flip (++)) drawingTip


canvasView : Canvas -> Html Msg
canvasView ({ width, height } as canvas) =
    toForms canvas
        |> Collage.collage (round width) (round height)
        |> GraphElement.toHtml
        |> List.singleton
        |> div
            [ MouseE.onMouseMove Hover
            , MouseE.onMouseUp <| always Lift
            , MouseE.onMouseDown Press
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
lift : Canvas -> Canvas
lift canvas =
    case canvas.state of
        Drawing (Just stroke) ->
            { canvas
                | strokes = canvas.strokes ++ [ stroke ]
                , state = Selecting
            }

        _ ->
            canvas


hover : Point -> Canvas -> Canvas
hover { x, y } ({ state, strokeSize, color, width, height } as canvas) =
    let
        point =
            { x = x - width / 2, y = height / 2 - y }

        newstate =
            case state of
                Drawing Nothing ->
                    Drawing <| Just <| Stroke.new point color strokeSize

                Drawing (Just stroke) ->
                    Drawing <| Just <| Stroke.draw point stroke

                _ ->
                    Hovering point
    in
        { canvas | state = newstate }


type Msg
    = Hover Point
    | Lift
    | Press Point
    | ChangeColor Color
    | ChangePenSize Float


update : Msg -> Canvas -> Canvas
update msg canvas =
    case msg of
        Hover point ->
            hover point canvas

        Press point ->
            hover point { canvas | state = Drawing Nothing }

        Lift ->
            lift canvas

        ChangeColor newcolor ->
            { canvas | color = newcolor }

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize }
