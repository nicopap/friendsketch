module Chat.Room exposing (Room, subs, new, Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import WebSocket
import Chat.Message as Message
import Chat.Message exposing (Message)
import Chat.InputField as InputField


host = "127.0.0.1:9260"
protocol = "ws"
basename = "/chat"
prefix = protocol ++ "://" ++ host ++ basename


getUrl : String -> String
getUrl roomName =
    prefix ++ "/" ++ roomName ++ "/fetch"


postUrl : String -> String
postUrl roomName =
    prefix ++ "/" ++ roomName ++ "/submit"


type alias Room =
    { history : List Message
    , name : String
    , inputContent : String
    }


new : Room
new =
    Room [] "Buggy room" ""


type Msg
    = NewMessage String
    | UpdateInput String
    | SubmitInput
    | SendInput String


update : Msg -> Room -> ( Room, Cmd Msg )
update msg room =
    case msg of
        NewMessage content ->
            ( { room | history = Message.decode content :: room.history }
            , Cmd.none
            )

        UpdateInput newtext ->
            ( { room | inputContent = newtext }, Cmd.none )

        SubmitInput ->
            ( { room | inputContent = "" }
            , InputField.prepareMessage room.inputContent SendInput
            )

        SendInput tosend ->
            ( room
            , WebSocket.send (postUrl room.name) tosend
            )


subs : Room -> Sub Msg
subs room =
    Sub.batch
        [ WebSocket.listen (getUrl room.name) NewMessage
        , WebSocket.keepAlive (postUrl room.name)
        ]


view : Room -> Html Msg
view room =
    div [ class "room" ]
        (List.append
            (List.map Message.view room.history)
            [ input
                [ placeholder "Input guess here"
                , autofocus True
                , value room.inputContent
                , onEnter SubmitInput
                , onInput UpdateInput
                ]
                [ text room.inputContent ]
            ]
        )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
