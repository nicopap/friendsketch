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
    = GameEvent API.GameEvent
    | UpdateInput String
    | SubmitInput


type ChatCmd
    = Send API.ChatContent
    | UpdateScroll
    | DoNothing


new : List API.GameEvent -> Chat
new history =
    { history = List.reverse <| List.map Message.into history
    , inputContent = ""
    }


receive : API.GameEvent -> Msg
receive = GameEvent


update : Msg -> Chat -> ( Chat, ChatCmd )
update msg ({ history, inputContent } as chat) =
    case msg of
        GameEvent event ->
            ( { chat | history = history ++ [ Message.into event ] }
            , UpdateScroll
            )

        UpdateInput newText ->
            ( { chat | inputContent = newText }, DoNothing )

        SubmitInput ->
            API.validChatContent inputContent
              |> Maybe.map (\cmd -> ( { chat | inputContent = "" }, Send cmd))
              |> Maybe.withDefault ( chat, DoNothing )


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
            List.map Message.view chat.history
    in
        div [ id "chat" ]
            [ div [ id "messages" ] history
            , div [ id "chat-input" ] [ inputField ]
            ]
