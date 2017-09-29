module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Chat exposing (Chat)
import Art.Canvas as Canvas exposing (Canvas)
import Room exposing (Room)


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
    , canvas : Canvas
    , room : Room
    }


type Msg
    = Chatmsg Chat.Msg
    | Canvasmsg Canvas.Msg
    | OpenChat
    | Spectate


init : ( Model, Cmd Msg )
init =
    let
        initRoom =
            Room.new "127.0.0.1:9260/" "room" "Gibonus"
    in
        { chat = Nothing
        , canvas = Canvas.new <| Room.canvasUrl initRoom
        , room = initRoom
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        map modelmap cmdmap ( subfield, cmd ) =
            ( modelmap subfield, Cmd.map cmdmap cmd )
    in
        case msg of
            Canvasmsg canvasmsg ->
                map (\x -> { model | canvas = x }) Canvasmsg <|
                    Canvas.update canvasmsg model.canvas

            Chatmsg chatmsg ->
                case model.chat of
                    Nothing ->
                        ( model, Cmd.none )

                    Just chat ->
                        map (\x -> { model | chat = Just x }) Chatmsg <|
                            Chat.update chatmsg chat

            OpenChat ->
                { model | chat = Just <| Chat.new model.room }
                    ! []

            Spectate ->
                { model | canvas = Canvas.changeArtist (Just "someone") model.canvas }
                    ! []


subscriptions : Model -> Sub Msg
subscriptions { chat, canvas } =
    let
        chatsubs =
            case chat of
                Nothing ->
                    Sub.none

                Just chat ->
                    Sub.map Chatmsg <| Chat.subs chat

        canvassubs =
            Sub.map Canvasmsg <| Canvas.subs canvas
    in
        Sub.batch [ chatsubs, canvassubs ]


view : Model -> Html Msg
view { chat, canvas } =
    let
        chatbutton =
            Html.button [ onClick OpenChat ] [ Html.text "Open Chat" ]

        chatview =
            case chat of
                Nothing ->
                    chatbutton

                Just chat ->
                    Html.map Chatmsg <| Chat.view chat

        canvasview =
            Html.div []
                [ Html.button [ onClick Spectate ] [ Html.text "Spectate" ]
                , Html.map Canvasmsg <| Canvas.view canvas
                ]
    in
        Html.div []
            [ canvasview
            , chatview
            ]
