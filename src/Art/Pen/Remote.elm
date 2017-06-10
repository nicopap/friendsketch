module Art.Pen.Remote exposing (Msg, State, update, subs, newState)


type Msg
    = Hi


type State
    = Hello


update =
    never


subs : a -> a
subs x =
    x


newState : State
newState =
    Hello
