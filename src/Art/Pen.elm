module Art.Pen
    exposing
        ( update
        , updateInput
        , Input
        , Msg
        , lift
        , drawAt
        , drawAbsolute
        )

{-| Specifications that something must follow in order to be a Pen for drawing
on a Canvas.
-}

import Task
import Art.Box as Box
import Art.Canvas as Canvas exposing (Canvas)


type Msg
    = DrawAt Box.Point
    | DrawAbsolute Box.Point
    | Lift


emit : Msg -> Cmd Msg
emit msg =
    Task.perform identity (Task.succeed msg)


{-| Lift the pen from the Canvas.
-}
lift : Cmd Msg
lift =
    emit Lift


{-| Draw on the Canvas on the given Point.
-}
drawAt : Box.Point -> Cmd Msg
drawAt point =
    emit (DrawAt point)


{-| Draw on the Canvas on the given Point.
-}
drawAbsolute : Box.Point -> Cmd Msg
drawAbsolute point =
    emit (DrawAbsolute point)


{-| The pen implementation.
-}
type alias Input s m =
    { state : s
    , update : m -> s -> ( s, Cmd Msg )
    , subs : s -> Sub m
    }


{-| Run the input's update function and transforms its state
-}
updateInput : m -> Input s m -> ( Input s m, Cmd Msg )
updateInput msg input =
    input.update msg input.state
        |> \( state, cmd ) -> ( { input | state = state }, cmd )


{-| The update to apply effects in the elm runtime.

This should only be used once, in the Art.elm module.

-}
update : Msg -> Canvas -> Canvas
update msg canvas =
    case msg of
        DrawAt point ->
            Canvas.draw point canvas

        DrawAbsolute point ->
            Canvas.drawAbsolute point canvas

        Lift ->
            Canvas.lift canvas
