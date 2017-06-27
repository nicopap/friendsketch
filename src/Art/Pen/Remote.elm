module Art.Pen.Remote exposing (newInput, State, Msg)

import Art.Canvas as Canvas

type Msg
    = Hi


type State
    = Hello


newInput : Canvas.Input State Msg
newInput =
    { state = Hello
    , update = (\m s -> ( s, Canvas.lift ))
    , subs = (\m -> Sub.none)
    }
