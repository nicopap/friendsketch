module Lobby.Create exposing (view, update, new, subs, Msg, Model)



type Msg
    = ()


type alias Model =
    ()


new : Model
new =
    Model


update : Model -> (Model, Cmd Msg)
update model =
    never


subs : Model -> Sub Msg
subs model =
    never


view : Model -> Html Msg
view model =
    never

