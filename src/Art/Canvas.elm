module Art.Canvas exposing (Msg, Canvas, new, view, update, subs, changeArtist)

import Debug
import Tuple exposing (mapFirst)
import Json.Encode as Encode exposing (encode)
import Json.Decode as Decode exposing (Decoder, decodeString)
import Color exposing (Color)
import ColorMath as ColMath
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


type Pen
    = Drawing Stroke
    | Hovering Point
    | Away


type Msg
    = Client ClientMsg
    | Server ServerMsg


type ServerMsg
    = Start Point Color Float
    | Continue Point
    | End
    | DecodeError String


type ClientMsg
    = Hover Point
    | Lift
    | Press Point
    | LeaveCanvas
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
        canvasCoords : Point -> Point
        canvasCoords { x, y } =
            { x = x - width / 2, y = height / 2 - y }

        properties =
            case currentArtist of
                Nothing ->
                    [ MouseE.onMouseMove (canvasCoords >> Hover)
                    , MouseE.onMouseUp (always Lift)
                    , MouseE.onMouseDown (canvasCoords >> Press)
                    , HEvents.onMouseOut LeaveCanvas
                    , id "drawingcontainer"
                    ]

                Just artistName ->
                    [ id "drawingcontainer"

                    --, id "inactive"
                    ]
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
            , WebSocket.send roomname (continueEncoder point)
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
    case pen of
        Hovering _ ->
            ( { canvas | pen = Drawing (Stroke.new point color strokeSize) }
            , WebSocket.send wsurl (startEncoder point color strokeSize)
            )

        Away ->
            (Debug.log "Impossible press message!!!" canvas) ! []

        Drawing _ ->
            (Debug.log "Impossible press message!!!" canvas) ! []


leave : Canvas -> Canvas
leave canvas =
    case canvas.pen of
        Drawing stroke ->
            lift canvas |> (\c -> { c | pen = Away })

        Hovering _ ->
            { canvas | pen = Away }

        Away ->
            Debug.log "Impossible leave message" canvas


clientUpdate : ClientMsg -> Canvas -> ( Canvas, Cmd msg )
clientUpdate msg canvas =
    case msg of
        Hover point ->
            clientHover point canvas

        Press point ->
            clientPress point canvas

        Lift ->
            lift canvas
                ! [ WebSocket.send canvas.wsurl endEncoder ]

        ChangeColor newcolor ->
            { canvas | color = newcolor } ! []

        ChangePenSize newsize ->
            { canvas | strokeSize = newsize } ! []

        LeaveCanvas ->
            leave canvas
                ! [ WebSocket.send canvas.wsurl endEncoder ]


serverUpdate : ServerMsg -> Canvas -> Canvas
serverUpdate msg canvas =
    case msg of
        Start point color size ->
            { canvas
                | strokeSize = size
                , color = color
                , pen = Drawing (Stroke.new point color size)
            }

        Continue point ->
            let
                continue =
                    case canvas.pen of
                        Drawing stroke ->
                            Drawing (Stroke.draw point stroke)

                        _ ->
                            canvas.pen
            in
                { canvas | pen = continue }

        End ->
            leave canvas

        DecodeError errorText ->
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
            WebSocket.listen wsurl
                (decodeString canvasActionDecoder >> handleCanvasAction)

        Nothing ->
            WebSocket.keepAlive wsurl



-- DECODERS


handleCanvasAction : Result String ServerMsg -> Msg
handleCanvasAction parsedMsg =
    case parsedMsg of
        Ok remoteMsg ->
            Server remoteMsg

        Err err ->
            Server (DecodeError err)


pointDecoder : Decoder Point
pointDecoder =
    Decode.map2 Point
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


colorDecoder : Decoder Color
colorDecoder =
    Decode.map
        (\x -> Result.withDefault Color.black (ColMath.hexToColor x))
        Decode.string


startDecoder : Decoder ServerMsg
startDecoder =
    Decode.map3 Start
        (Decode.index 0 pointDecoder)
        (Decode.index 1 colorDecoder)
        (Decode.index 2 Decode.float)


startEncoder : Point -> Color -> Float -> String
startEncoder { x, y } color size =
    Encode.list
        [ Encode.list [ Encode.float x, Encode.float y ]
        , Encode.string <| ColMath.colorToHex color
        , Encode.float size
        ]
        |> (,) "start"
        |> List.singleton
        |> Encode.object
        |> encode 0


continueEncoder : Point -> String
continueEncoder { x, y } =
    encode 0 <|
        Encode.object
            [ ( "continue", Encode.list [ Encode.float x, Encode.float y ] ) ]


endEncoder : String
endEncoder =
    encode 0 <| Encode.object [ ( "end", Encode.null ) ]


canvasActionDecoder : Decoder ServerMsg
canvasActionDecoder =
    Decode.oneOf
        [ Decode.field "start" startDecoder
        , Decode.field "continue" <| Decode.map Continue pointDecoder
        , Decode.field "end" <| Decode.succeed End
        ]
