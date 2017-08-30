module Index exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Chat exposing (Chat)
import Art.Canvas as Canvas exposing (Canvas)


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
    }


type Msg
    = Chatmsg Chat.Msg
    | Canvasmsg Canvas.Msg
    | OpenChat


init : ( Model, Cmd Msg )
init =
    { chat = Nothing
    , canvas = Canvas.new
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
                ( { model | canvas = Canvas.update canvasmsg model.canvas }
                , Cmd.none
                )

            Chatmsg chatmsg ->
                case model.chat of
                    Nothing ->
                        ( model, Cmd.none )

                    Just chat ->
                        map (\x -> { model | chat = Just x }) Chatmsg <|
                            Chat.update chatmsg chat

            OpenChat ->
                ( { model | chat = Just <| Chat.new "127.0.0.1:9260/chat/" "room" }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions { chat } =
    case chat of
        Nothing ->
            Sub.none

        Just chat ->
            Sub.map Chatmsg <| Chat.subs chat


view : Model -> Html Msg
view { chat, canvas } =
    let
        chatbutton =
            Html.button [ onClick OpenChat ] [ Html.text "Open Chat" ]
    in
        Html.div []
            [ Html.map Canvasmsg <| Canvas.view canvas
            , case chat of
                Nothing ->
                    chatbutton

                Just chat ->
                    Html.map Chatmsg <| Chat.view chat
            ]
