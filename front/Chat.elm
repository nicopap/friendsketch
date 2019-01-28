module Chat exposing (Chat, new, Msg, ChatCmd(..), update, view, receive)

import API
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value, autofocus, placeholder, id)
import Html.Events exposing (onInput)
import Chat.Message as Message exposing (Message)
import Chat.InputField exposing (onEnter)


type alias Chat =
    { history : List Message
    , inputContent : String
    -- TODO: , ghost : List OwnMessage
    }


type Msg
    = NewMessage API.ChatMsg_
    | UpdateInput String
    | SubmitInput


type ChatCmd
    = Send API.ChatContent
    | UpdateScroll
    | DoNothing


new : List API.ChatMsg_ -> Chat
new history =
    { history = List.map Message.into history
    , inputContent = ""
    }


receive : API.ChatMsg_ -> Msg
receive = NewMessage


update : Msg -> Chat -> ( Chat, ChatCmd )
update msg ({ history, inputContent } as chat) =
    case msg of
        NewMessage message ->
            ( { chat | history = Message.insert message history }
            , UpdateScroll
            )

        UpdateInput newText ->
            ( { chat | inputContent = newText }, DoNothing )

        SubmitInput ->
            ( { chat | inputContent = "" }
            , API.validChatContent inputContent
                |> Maybe.map Send
                |> Maybe.withDefault DoNothing
            )



view : Chat -> Html Msg
view chat =
    let
        inputField =
            input
                [ placeholder "Input guess here"
                , autofocus True
                , value chat.inputContent
                , onEnter SubmitInput
                , onInput UpdateInput
                ]
                [ text chat.inputContent ]

        history =
            List.map Message.view <| List.reverse chat.history
    in
        div [ id "chat" ]
            [ div [ id "messages" ] history
            , div [ id "chat-input" ] [ inputField ]
            ]
