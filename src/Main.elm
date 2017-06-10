module Main exposing (main)

import Json.Decode exposing (Decoder)
import Html exposing (..)
import Chat
import Art


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model Chat.new Art.new, Cmd.none )



-- Model


type alias Model =
    { chat : Chat.Chat
    , art : Art.Art
    }



-- Subcriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Chatmsg (Chat.subs model.chat)
        , Sub.map Artmsg (Art.subs model.art)
        ]


type Msg
    = Chatmsg Chat.Msg
    | Artmsg Art.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mapart : ( Art.Art, Cmd Art.Msg ) -> ( Model, Cmd Msg )
        mapart artcmd =
            ( { model | art = Tuple.first artcmd }
            , Cmd.map Artmsg (Tuple.second artcmd)
            )

        mapchat : ( Chat.Chat, Cmd Chat.Msg ) -> ( Model, Cmd Msg )
        mapchat chatcmd =
            ( { model | chat = Tuple.first chatcmd }
            , Cmd.map Chatmsg (Tuple.second chatcmd)
            )
    in
        case msg of
            Artmsg artmsg ->
                mapart (Art.update artmsg model.art)

            Chatmsg chatmsg ->
                mapchat (Chat.update chatmsg model.chat)


view : Model -> Html Msg
view model =
    span []
        [ Html.map Artmsg (Art.view model.art)
        , Html.map Chatmsg (Chat.view model.chat)
        ]
