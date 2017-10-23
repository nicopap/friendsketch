module Art.Canvas
    exposing
        ( Msg
        , Canvas
        , State(..)
        , new
        , view
        , update
        , subs
        )

import Tuple exposing (mapFirst, mapSecond)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Html.Events as HEvents
import Debug
import Collage
import Element as GraphElement
import ElementRelativeMouseEvents as MouseE exposing (Point)
import List.Nonempty as NE exposing (Nonempty)
import Art.Stroke as Stroke exposing (Stroke)
import Art.Toolbox as Toolbox
import API


type Pen
    = Drawing Stroke
    | Hovering Point
    | Away


type Msg
    = ArtistMsg ArtistMsg
    | Server (Result String API.CanvasMsg)


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
    , wslisten : Maybe (Result String API.CanvasMsg -> Msg) -> Sub Msg
    , wssend : API.CanvasMsg -> Cmd Msg
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
    , wslisten = API.wscanvasListen game roomid name
    , wssend = API.wscanvasSend game roomid name
    }



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
            id "drawingcontainer"
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
view canvas =
    Html.map ArtistMsg <|
        div []
            [ canvasView canvas
            , div [ id "toolbox" ] [ Toolbox.view ChangeColor ChangePenSize ]
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

        Away ->
            Debug.log "Impossible lift message!!!" canvas

        Hovering _ ->
            Debug.log "Impossible lift message!!!" canvas


{-| Modifies pen according to the new hover point.
If the new point is worth sending to the server, also returns the input
point.

Note: if the line is currently being draw, it adds to it, if the client
is just hovering over the canvas, then it adjusts the location of the
hovering mark.

-}
hover : Point -> Pen -> ( Pen, Maybe Point )
hover point pen =
    case pen of
        Drawing stroke ->
            case Stroke.drawFeedback point stroke of
                Just newstroke ->
                    ( Drawing newstroke, Just point )

                Nothing ->
                    ( Drawing stroke, Nothing )

        Hovering _ ->
            ( Hovering point, Nothing )

        Away ->
            ( Hovering point, Nothing )


{-| What happens when a client goes from unpressed to pressed pen.
-}
press : Point -> Color -> Float -> Pen
press point color strokeSize =
    Drawing (Stroke.new point color strokeSize)


{-| Send `Just point` to server, otherwise Nothing
-}
continueSend : (API.CanvasMsg -> Cmd msg) -> Maybe Point -> Cmd msg
continueSend action maybePoint =
    case maybePoint of
        Nothing ->
            Cmd.none

        Just point ->
            action <| API.CnvContinue point


{-| Update function when the canvas state is `Artist`.
-}
artistUpdate : ArtistMsg -> Canvas -> ( Canvas, Cmd Msg )
artistUpdate msg canvas =
    let
        clientPress point pen =
            { canvas | pen = pen }
                ! [ canvas.wssend <|
                        API.CnvStart point canvas.color canvas.strokeSize
                  ]

        clientHover point =
            hover point canvas.pen
                |> mapFirst (\p -> { canvas | pen = p })
                |> mapSecond (continueSend canvas.wssend)
    in
        case msg of
            Hover point ->
                clientHover point

            Press point ->
                press point canvas.color canvas.strokeSize
                    |> clientPress point

            Lift ->
                lift canvas ! [ canvas.wssend API.CnvEnd ]

            ChangeColor newcolor ->
                { canvas | color = newcolor } ! []

            ChangePenSize newsize ->
                { canvas | strokeSize = newsize } ! []

            LeaveCanvas ->
                (lift canvas |> (\c -> { c | pen = Away }))
                    ! [ canvas.wssend API.CnvEnd ]

            EnterCanvas ( point, True ) ->
                press point canvas.color canvas.strokeSize
                    |> clientPress point

            EnterCanvas ( point, False ) ->
                clientHover point


serverUpdate : Result String API.CanvasMsg -> Canvas -> Canvas
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
            Ok (API.CnvStart point color size) ->
                { canvas
                    | strokeSize = size
                    , color = color
                    , pen = Drawing (Stroke.new point color size)
                }

            Ok (API.CnvContinue point) ->
                { canvas | pen = continueDrawing point canvas.pen }

            Ok API.CnvEnd ->
                lift canvas |> (\c -> { c | pen = Away })

            Err errorText ->
                Debug.log ("Issue in server message: " ++ errorText) canvas


{-| A version of the canvas that draws client-side actions but do
not send information to the server.
-}
pregameUpdate : ArtistMsg -> Canvas -> Canvas
pregameUpdate msg canvas =
    case msg of
        Hover point ->
            hover point canvas.pen
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
            hover point canvas.pen
                |> (\( pen, _ ) -> { canvas | pen = pen })


update : Msg -> Canvas -> ( Canvas, Cmd Msg )
update msg canvas =
    case ( msg, canvas.state ) of
        ( Server msg_, Spectator ) ->
            serverUpdate msg_ canvas ! []

        ( ArtistMsg msg_, Artist ) ->
            artistUpdate msg_ canvas

        ( ArtistMsg msg_, Pregame ) ->
            pregameUpdate msg_ canvas ! []

        ( msg_, state ) ->
            Debug.log
                ("Inconsistent state between server and client!")
                (canvas)
                ! []


subs : Canvas -> Sub Msg
subs { wslisten, state } =
    case state of
        Spectator ->
            wslisten <| Just Server

        Artist ->
            wslisten Nothing

        Pregame ->
            wslisten Nothing
