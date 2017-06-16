module Art.Pen
    exposing
        ( Msg(DrawAt, DrawAbsolute, Lift, UpdatePosition)
        , update
        , subs
        )

{-| Specifications that something must follow in order to be a Pen for drawing
on a Canvas.


# Messages

@docs Msg


# Update

@docs update

-}

import Art.Box as Box
import Art.Canvas as Canvas
import Art.Canvas exposing (Canvas)


{-| The messages that a Pen must emit.

This is then captured by the update function to dispatch the canvas operation.

    type Msg
        = DrawAt Box.Point
        | DrawAbsolute Box.Point
        | Lift
        | UpdatePosition

Each message but UpdatePosition has a corresponding function in the Canvas
module.

UpdatePosition refreshes the knowledge the location of the Canvas on the
webpage. This is done to avoid overhead when drawing a bunch of points at
the same time.

-}
type Msg
    = DrawAt Box.Point
    | DrawAbsolute Box.Point
    | Lift
    | UpdatePosition
    | UpdatePosition_ Box.Box


{-| The update to apply effects in the elm runtime.

This should only be used once, in the Art.elm module.

-}
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
