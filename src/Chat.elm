module Chat exposing (Chat, subs, new, Msg, update, view)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Chat.Message as Message exposing (Message)
import Chat.InputField as InputField


type alias Chat =
    { roomid : String
    , baseurl : String
    , history : List Message
    , inputContent : String
    }


type Msg
    = NewMessage String
    | UpdateInput String
    | SubmitInput
    | SendInput String


chatUrl : Chat -> String
chatUrl { roomid, baseurl } =
    "ws://" ++ baseurl ++ roomid


subs : Chat -> Sub Msg
subs chat =
    Sub.batch
        [ WebSocket.listen (chatUrl chat) NewMessage
        ]


new : String -> String -> Chat
new baseurl roomid =
    { roomid = roomid
    , baseurl = baseurl
    , history = []
    , inputContent = ""
    }


update : Msg -> Chat -> ( Chat, Cmd Msg )
update msg chat =
    case msg of
        NewMessage content ->
            { chat | history = Message.decode content :: chat.history } ! []

        UpdateInput newtext ->
            { chat | inputContent = newtext } ! []

        SubmitInput ->
            { chat | inputContent = "" }
                ! [ InputField.prepareMessage chat.inputContent SendInput
                  ]

        SendInput tosend ->
            chat ! [ WebSocket.send (chatUrl chat) tosend ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" <| Json.andThen isEnter keyCode


view : Chat -> Html Msg
view chat =
    div [ class "chat" ]
        (List.map Message.view chat.history
            ++ [ input
                    [ placeholder "Input guess here"
                    , autofocus True
                    , value chat.inputContent
                    , onEnter SubmitInput
                    , onInput UpdateInput
                    ]
                    [ text chat.inputContent ]
               ]
        )
