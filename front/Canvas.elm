module Canvas
    exposing
        ( Msg
        , Canvas
        , State(..)
        , new
        , view
        , update
        , subsAdaptor
        )

import Tuple exposing (mapFirst, mapSecond)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Html.Events as HEvents
import Task
import Debug
import Collage
import Element as GraphElement
import ElementRelativeMouseEvents as MouseE exposing (Point)
import List.Nonempty as NE exposing (Nonempty)
import Canvas.Stroke as Stroke exposing (Stroke)
import Canvas.Toolbox as Toolbox exposing (Toolbox)
import API


type Pen
    = Drawing Stroke
    | Hovering Point
    | Away


type Msg
    = ArtistMsg ArtistMsg
    | Server API.CanvasMsg
    | ToolboxMsg Toolbox.Msg


type ArtistMsg
    = Hover Point
    | Lift
    | Press Point
    | LeaveCanvas
    | EnterCanvas ( Point, Bool )
    | ChangeColor Color
    | ChangePenSize Float


{-| Controls the canvas behavior.

The canvas knows whether to send and allow drawing with its state:

  - `Spectator` : Cannot draw, draws what the server asks to draw.
  - `Artist` : Can draw, will send to the server the strokes.
  - `Pregame` : Can draw, but do not send strokes to the server. This is to
    fill time when the user cannot draw or see someone else draw.

-}
type State
    = Spectator
    | Artist
    | Pregame


type alias Canvas =
    { strokes : List Stroke
    , state : State
    , pen : Pen
    , color : Color
    , strokeSize : Float
    , width : Float
    , height : Float
    , toolbox : Toolbox
    }


{-| An initial canvas.
-}
new : API.Game -> API.RoomID -> API.Name -> State -> Canvas
new game roomid name state =
    { strokes = []
    , state = state
    , pen = Away
    , color = Color.black
    , strokeSize = 20
    , width = 600
    , height = 400
    , toolbox = Toolbox.new
    }


cmd : msg -> Cmd msg
cmd msg =
    Task.perform identity <| Task.succeed msg



-- VIEW


{-| Displays a stroke.
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


{-| Display the outline of the brush when hovering over the canvas
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
toForms { pen, strokeSize, strokes, color } =
    let
        drawingTip =
            case pen of
                Hovering { x, y } ->
                    [ brushOutline strokeSize color ( x, y ) ]

                Drawing stroke ->
                    [ strokeToForm stroke ]

                Away ->
                    []
    in
        strokes
            |> List.map strokeToForm
            |> (flip (++)) drawingTip


canvasView : Canvas -> Html ArtistMsg
canvasView ({ width, height, state } as canvas) =
    let
        canvasXY : Point -> Point
        canvasXY { x, y } =
            { x = x - width / 2, y = height / 2 - y }

        properties : List (Html.Attribute ArtistMsg)
        properties =
            id "canvas"
                :: case state of
                    Spectator ->
                        []

                    _ ->
                        [ MouseE.onMouseMove (canvasXY >> Hover)
                        , MouseE.onMouseUp (always Lift)
                        , MouseE.onMouseDown (canvasXY >> Press)
                        , MouseE.onMouseEnter (mapFirst canvasXY >> EnterCanvas)
                        , HEvents.onMouseLeave LeaveCanvas
                        ]
    in
        toForms canvas
            |> Collage.collage (round width) (round height)
            |> GraphElement.toHtml
            |> List.singleton
            |> div properties


view : Canvas -> Html Msg
view ({ toolbox, state, strokes, pen } as canvas) =
    let
        adaptToolbox result =
            case result of
                Err toolboxmsg ->
                    ToolboxMsg toolboxmsg

                Ok canvasmsg ->
                    ArtistMsg canvasmsg

        stateClass =
            case state of
                Artist ->
                    if strokes == [] && pen == Away then
                        "artist-empty artist"
                    else
                        "artist"

                Pregame ->
                    "pregame"

                Spectator ->
                    "spectator"
    in
        div [ id "artcontainer", class (stateClass ++ " top-layout") ]
            [ Html.map ArtistMsg <| canvasView canvas
            , Html.map adaptToolbox <|
                Toolbox.view ChangeColor ChangePenSize toolbox
            ]



-- UPDATE


{-| Lift the "pen" from the canvas, that means the stroke is finished
-}
lift : Canvas -> Canvas
lift ({ strokes, pen } as canvas) =
    case pen of
        Drawing stroke ->
            { canvas
                | strokes = strokes ++ [ stroke ]
                , pen = Hovering (Stroke.head stroke)
            }

        _ ->
            canvas


{-| Modifies pen according to the new hover point.
If the new point is worth sending to the server, also returns the input
point.

Note: if the line is currently being draw, it adds to it, if the client
is just hovering over the canvas, then it adjusts the location of the
hovering mark.

-}
move : Point -> Pen -> ( Pen, Maybe Point )
move point pen =
    case pen of
        Hovering _ ->
            ( Hovering point, Nothing )

        Away ->
            ( Hovering point, Nothing )

        Drawing stroke ->
            case Stroke.drawFeedback point stroke of
                Just newstroke ->
                    ( Drawing newstroke, Just point )

                Nothing ->
                    ( Drawing stroke, Nothing )


{-| What happens when a client goes from unpressed to pressed pen.
-}
press : Point -> Color -> Float -> Pen
press point color strokeSize =
    Drawing (Stroke.new point color strokeSize)


{-| Update function when the canvas state is `Artist`.
-}
artistUpdate : ArtistMsg -> Canvas -> ( Canvas, Maybe API.CanvasMsg )
artistUpdate msg canvas =
    let
        clientPress point pen =
            ( { canvas | pen = pen }
            , Just <| API.CnvStart point canvas.color canvas.strokeSize
            )

        clientMove point =
            move point canvas.pen
                |> mapFirst (\p -> { canvas | pen = p })
                |> mapSecond (Maybe.map API.CnvContinue)
    in
        case msg of
            Hover point ->
                clientMove point

            Press point ->
                press point canvas.color canvas.strokeSize
                    |> clientPress point

            Lift ->
                ( lift canvas, Just API.CnvEnd )

            ChangeColor newcolor ->
                ( { canvas | color = newcolor }, Nothing )

            ChangePenSize newsize ->
                ( { canvas | strokeSize = newsize }, Nothing )

            EnterCanvas ( point, True ) ->
                press point canvas.color canvas.strokeSize
                    |> clientPress point

            EnterCanvas ( point, False ) ->
                clientMove point

            LeaveCanvas ->
                case canvas.pen of
                    Drawing _ ->
                        ( lift canvas |> (\c -> { c | pen = Away })
                        , Just API.CnvEnd
                        )

                    _ ->
                        ( { canvas | pen = Away }, Nothing )


serverUpdate : API.CanvasMsg -> Canvas -> Canvas
serverUpdate msg canvas =
    let
        continueDrawing point pen =
            case pen of
                Drawing stroke ->
                    Drawing (Stroke.draw point stroke)

                _ ->
                    pen
    in
        case msg of
            API.CnvStart point color size ->
                { canvas
                    | strokeSize = size
                    , color = color
                    , pen = Drawing (Stroke.new point color size)
                }

            API.CnvContinue point ->
                { canvas | pen = continueDrawing point canvas.pen }

            API.CnvEnd ->
                lift canvas |> (\c -> { c | pen = Away })


{-| A version of the canvas that draws client-side actions but do
not send information to the server.
-}
pregameUpdate : ArtistMsg -> Canvas -> Canvas
pregameUpdate msg canvas =
    case msg of
        Hover point ->
            move point canvas.pen
                |> (\( pen, _ ) -> { canvas | pen = pen })

        Press point ->
            { canvas | pen = press point canvas.color canvas.strokeSize }

        Lift ->
            lift canvas

        ChangeColor newcolor ->
            { canvas | color = newcolor }

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize }

        LeaveCanvas ->
            lift canvas
                |> (\c -> { c | pen = Away })

        EnterCanvas ( point, True ) ->
            { canvas | pen = press point canvas.color canvas.strokeSize }

        EnterCanvas ( point, False ) ->
            move point canvas.pen
                |> (\( pen, _ ) -> { canvas | pen = pen })


update : Msg -> Canvas -> ( Canvas, Maybe API.CanvasMsg )
update msg canvas =
    let
        updateToolbox msg_ =
            let
                ( toolbox, color ) =
                    Toolbox.update msg_ canvas.toolbox
            in
                { canvas | toolbox = toolbox }
                    |> artistUpdate (ChangeColor color)
    in
        case ( msg, canvas.state ) of
            ( Server msg_, Spectator ) ->
                ( serverUpdate msg_ canvas, Nothing )

            ( ToolboxMsg msg_, Artist ) ->
                updateToolbox msg_

            ( ToolboxMsg msg_, Pregame ) ->
                updateToolbox msg_

            ( ArtistMsg msg_, Artist ) ->
                artistUpdate msg_ canvas

            ( ArtistMsg msg_, Pregame ) ->
                ( pregameUpdate msg_ canvas, Nothing )

            ( msg_, state ) ->
                ( Debug.log "Inconsistent state!" canvas, Nothing )


subsAdaptor : Canvas -> Maybe (API.CanvasMsg -> Msg)
subsAdaptor { state } =
    case state of
        Spectator ->
            Just Server

        Artist ->
            Nothing

        Pregame ->
            Nothing
