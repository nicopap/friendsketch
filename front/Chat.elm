module Chat exposing (Chat, subs, new, Msg, update, view)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NeatSocket
import Chat.Message as Message exposing (Message)
import Chat.InputField as InputField
import Room exposing (Room)


type alias Chat =
    { room : Room
    , history : List Message
    , inputContent : String
    }


type Msg
    = NewMessage String
    | UpdateInput String
    | SubmitInput
    | SendInput String


subs : Chat -> Sub Msg
subs { room } =
    Sub.batch
        [ NeatSocket.listen (Room.chatUrl room) NewMessage
        ]


new : Room -> Chat
new room =
    { room = room
    , history = []
    , inputContent = ""
    }


update : Msg -> Chat -> ( Chat, Cmd Msg )
update msg chat =
    case msg of
        NewMessage content ->
            ( { chat | history = chat.history ++ [ Message.decode content ] }
            , Cmd.none
            )

        UpdateInput newtext ->
            ( { chat | inputContent = newtext }
            , Cmd.none
            )

        SubmitInput ->
            ( { chat | inputContent = "" }
            , InputField.prepareMessage chat.inputContent SendInput
            )

        SendInput tosend ->
            ( chat
            , NeatSocket.send (Room.chatUrl chat.room) tosend
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
