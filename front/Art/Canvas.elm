module Art.Canvas exposing (Msg, Canvas, new, view, update, subs, changeArtist)

import Debug
import Tuple exposing (mapFirst)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Html.Events as HEvents
import WebSocket
import Collage
import Element as GraphElement
import ElementRelativeMouseEvents as MouseE exposing (Point)
import List.Nonempty as NE exposing (Nonempty)
import Art.Stroke as Stroke exposing (Stroke)
import Art.Toolbox as Toolbox
import Art.Canvas.ServerMsgDecoder as ServerMsg exposing (ServerMsg)


type Pen
    = Drawing Stroke
    | Hovering Point
    | Away


type Msg
    = Client ClientMsg
    | Server ServerMsg


type ClientMsg
    = Hover Point
    | Lift
    | Press Point
    | LeaveCanvas
    | EnterCanvas ( Point, Bool )
    | ChangeColor Color
    | ChangePenSize Float


type alias Canvas =
    { strokes : List Stroke
    , currentArtist : Maybe String
    , pen : Pen
    , color : Color
    , strokeSize : Float
    , width : Float
    , height : Float
    , wsurl : String
    }


{-| A default canvas
-}
new : String -> Canvas
new =
    Canvas [] Nothing Away Color.black 20 600 400


changeArtist : Maybe String -> Canvas -> Canvas
changeArtist newartist canvas =
    { canvas | currentArtist = newartist }



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


canvasView : Canvas -> Html ClientMsg
canvasView ({ width, height, currentArtist } as canvas) =
    let
        canvasXY : Point -> Point
        canvasXY { x, y } =
            { x = x - width / 2, y = height / 2 - y }

        properties : List (Html.Attribute ClientMsg)
        properties =
            id "drawingcontainer"
                :: case currentArtist of
                    Nothing ->
                        [ MouseE.onMouseMove (canvasXY >> Hover)
                        , MouseE.onMouseUp (always Lift)
                        , MouseE.onMouseDown (canvasXY >> Press)
                        , MouseE.onMouseEnter (mapFirst canvasXY >> EnterCanvas)
                        , HEvents.onMouseLeave LeaveCanvas
                        ]

                    Just artistName ->
                        []
    in
        toForms canvas
            |> Collage.collage (round width) (round height)
            |> GraphElement.toHtml
            |> List.singleton
            |> div properties


view : Canvas -> Html Msg
view canvas =
    Html.map Client <|
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


drawFeedback : Point -> String -> Stroke -> ( Stroke, Cmd msg )
drawFeedback point roomname stroke =
    case Stroke.drawFeedback point stroke of
        Just newstroke ->
            ( newstroke
            , WebSocket.send roomname (ServerMsg.continueEncoder point)
            )

        Nothing ->
            ( stroke, Cmd.none )


clientHover : Point -> Canvas -> ( Canvas, Cmd msg )
clientHover point canvas =
    case canvas.pen of
        Drawing stroke ->
            drawFeedback point canvas.wsurl stroke
                |> mapFirst (\s -> { canvas | pen = Drawing s })

        Hovering _ ->
            { canvas | pen = Hovering point } ! []

        Away ->
            { canvas | pen = Hovering point } ! []


clientPress : Point -> Canvas -> ( Canvas, Cmd msg )
clientPress point ({ wsurl, pen, strokeSize, color, width, height } as canvas) =
    let
        pressedCanvas =
            ( { canvas | pen = Drawing (Stroke.new point color strokeSize) }
            , WebSocket.send wsurl <|
                ServerMsg.startEncoder point color strokeSize
            )
    in
        case pen of
            Hovering _ ->
                pressedCanvas

            Away ->
                pressedCanvas

            Drawing _ ->
                (Debug.log "Impossible press message!!!" canvas) ! []


clientLeave : Canvas -> ( Canvas, Cmd msg )
clientLeave canvas =
    case canvas.pen of
        Drawing stroke ->
            (lift canvas |> (\c -> { c | pen = Away }))
                ! [ WebSocket.send canvas.wsurl ServerMsg.endEncoder ]

        Hovering _ ->
            { canvas | pen = Away }
                ! []

        Away ->
            Debug.log "Impossible leave message" canvas
                ! []


clientUpdate : ClientMsg -> Canvas -> ( Canvas, Cmd msg )
clientUpdate msg canvas =
    case msg of
        Hover point ->
            clientHover point canvas

        Press point ->
            clientPress point canvas

        Lift ->
            lift canvas
                ! [ WebSocket.send canvas.wsurl ServerMsg.endEncoder ]

        ChangeColor newcolor ->
            { canvas | color = newcolor } ! []

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize } ! []

        LeaveCanvas ->
            clientLeave canvas

        EnterCanvas ( point, True ) ->
            clientPress point canvas

        EnterCanvas ( point, False ) ->
            clientHover point canvas


serverUpdate : ServerMsg -> Canvas -> Canvas
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
            ServerMsg.Start point color size ->
                { canvas
                    | strokeSize = size
                    , color = color
                    , pen = Drawing (Stroke.new point color size)
                }

            ServerMsg.Continue point ->
                { canvas | pen = continueDrawing point canvas.pen }

            ServerMsg.End ->
                (\c -> { c | pen = Away }) <| lift canvas

            ServerMsg.DecodeError errorText ->
                Debug.log ("Issue in server message: " ++ errorText) canvas


update : Msg -> Canvas -> ( Canvas, Cmd Msg )
update msg canvas =
    case msg of
        Server msg_ ->
            serverUpdate msg_ canvas ! []

        Client msg_ ->
            clientUpdate msg_ canvas


subs : Canvas -> Sub Msg
subs { wsurl, currentArtist } =
    case currentArtist of
        Just _ ->
            WebSocket.listen wsurl <| ServerMsg.decode Server

        Nothing ->
            WebSocket.keepAlive wsurl
