module Index exposing (main)

import Html exposing (span, Html)
import Html.Events exposing (onClick)
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
    { chat : Maybe Chat
    , art : Art
    }


type Msg
    = Chatmsg Chat.Msg
    | Artmsg Art.Msg
    | OpenChat


init : ( Model, Cmd Msg )
init =
    { chat = Nothing
    , art = Art.new
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        map modelmap cmdmap ( subfield, cmd ) =
            ( modelmap subfield, Cmd.map cmdmap cmd )
    in
        case msg of
            Artmsg artmsg ->
                map (\x -> { model | art = x }) Artmsg <|
                    Art.update artmsg model.art

            Chatmsg chatmsg ->
                case model.chat of
                    Nothing ->
                        ( model, Cmd.none )

                    Just chat ->
                        map (\x -> { model | chat = Just x }) Chatmsg <|
                            Chat.update chatmsg chat

            OpenChat ->
                { model | chat = Just <| Chat.new "127.0.0.1:9260/chat/" "room" }
                    ! []


subscriptions : Model -> Sub Msg
subscriptions { art, chat } =
    Sub.batch
        [ case chat of
            Nothing ->
                Sub.none

            Just chat ->
                Sub.map Chatmsg <| Chat.subs chat
        , Sub.map Artmsg <| Art.subs art
        ]


view : Model -> Html Msg
view { chat, art } =
    let
        chatbutton =
            Html.button [ onClick OpenChat ] [ Html.text "Open Chat" ]
    in
        span []
            [ Html.map Artmsg <| Art.view art
            , case chat of
                Nothing ->
                    chatbutton

                Just chat ->
                    Html.map Chatmsg <| Chat.view chat
            ]
