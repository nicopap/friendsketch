module Art.Pen.Remote exposing (newInput, State, Msg)

import Art.Pen as Pen


type Msg
    = Hi


type State
    = Hello


newInput : Pen.Input State Msg
newInput =
    { state = Hello
    , update = (\m s -> ( s, Pen.lift ))
    , subs = (\m -> Sub.none)
    }
