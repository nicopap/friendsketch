module Main exposing (main)

import Html exposing (span, Html)
import Chat exposing (Chat)
import Art exposing (Art)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { chat : Chat
    , art : Art
    }


type Msg
    = Chatmsg Chat.Msg
    | Artmsg Art.Msg


init : ( Model, Cmd Msg )
init =
    ( { chat = Chat.new, art = Art.new }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Artmsg artmsg ->
            let
                map ( art, cmd ) =
                    ( { model | art = art }, Cmd.map Artmsg cmd )
            in
                map <| Art.update artmsg model.art

        Chatmsg chatmsg ->
            let
                map ( chat, cmd ) =
                    ( { model | chat = chat }, Cmd.map Chatmsg cmd )
            in
                map <| Chat.update chatmsg model.chat


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Chatmsg <| Chat.subs model.chat
        , Sub.map Artmsg <| Art.subs model.art
        ]


view : Model -> Html Msg
view model =
    span []
        [ Html.map Artmsg <| Art.view model.art
        , Html.map Chatmsg <| Chat.view model.chat
        ]
