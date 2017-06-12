module Art.Pen exposing (Msg(..), update, subs)

{-| Specifications that something must follow in order to be a Pen for drawing
that can draw on a Canvas
-}

import Art.Box as Box
import Art.Canvas as Canvas
import Art.Canvas exposing (Canvas)


{-| The messages that a Pen must emit.
-}
type Msg
    = DrawAt Box.Point
    | DrawAbsolute Box.Point
    | Lift
    | UpdatePosition
    | UpdatePosition_ Box.Box


update : Msg -> Canvas -> ( Canvas, Cmd Msg )
update msg canvas =
    case msg of
        DrawAt point ->
            ( Canvas.draw point canvas, Cmd.none )

        DrawAbsolute point ->
            ( Canvas.drawAbsolute point canvas, Cmd.none )

        Lift ->
            ( Canvas.lift canvas, Cmd.none )

        UpdatePosition ->
            ( canvas, Box.checkCanvasBox () )

        UpdatePosition_ newbox ->
            let
                oldbox =
                    canvas.box

                newbox_ =
                    Box.Box newbox.x newbox.y oldbox.width oldbox.height
            in
                ( { canvas | box = newbox_ }, Cmd.none )


subs : Sub Msg
subs =
    Box.sub UpdatePosition_
