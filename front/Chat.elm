module Chat exposing (Chat, new, Msg, ChatCmd(..), update, view, receive)

import Api
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value, autofocus, placeholder, id, class)
import Html.Events exposing (onInput)
import Chat.InputField exposing (onEnter)


type Message
    = Message_ { author : String , content : String }
    | Ghost { content : String }
    | Left String
    | Joined String
    | Start String
    | Over String
    | Guessed String


type alias Chat =
    { history : List Message
    , inputContent : String
    , user : Api.Name
    , pending : List Api.ChatContent
    }


type Msg
    = Event Api.VisibleEvent
    | UpdateInput String
    | SubmitInput


type ChatCmd
    = Send Api.ChatContent
    | UpdateScroll
    | DoNothing


into : Api.VisibleEvent -> Message
into event =
    case event of
        Api.EvLeft name ->
            Left <| Api.showName name

        Api.EvJoined name ->
            Joined <| Api.showName name

        Api.EvMessage { content, author } ->
            Message_
                { content = Api.showChatContent content
                , author = Api.showName author
                }

        Api.EvOver word       -> Over word
        Api.EvStart artist    -> Start <| Api.showName artist
        Api.EvGuessed guesser -> Guessed <| Api.showName guesser


messageView : String -> Message -> Html msg
messageView self message =
    let
        genericMessage classes content =
            div [ class ("message " ++ classes) ] [ text content ]
    in
        case message of
            Ghost { content } ->
                genericMessage "own ghost" (self ++ ": "  ++ content)

            Message_ { author, content } ->
                let classes = if author == self then "text own" else "text"
                in  genericMessage classes (author ++ ": " ++ content)

            Left name ->
                genericMessage "leaves" (name ++ " left")

            Joined name ->
                genericMessage "join" (name ++ " joined")

            Guessed name ->
                genericMessage "correct-guess" (name ++ " guessed the word")

            Start name ->
                genericMessage "round-start" (name ++ " is going to draw")

            Over word ->
                genericMessage "round-over" ("round over, the word was " ++ word)


new : Api.Name -> List Api.VisibleEvent -> Chat
new user history =
    { history = List.reverse <| List.map into history
    , inputContent = ""
    , pending = []
    , user = user
    }


receive : Api.VisibleEvent -> Msg
receive = Event


updateHistory : Api.VisibleEvent -> Chat -> Chat
updateHistory event ({ history, pending, user } as chat_) =
    let
        chat =
            case event of
                Api.EvMessage { author, content } ->
                    if author == user then
                        { chat_ | pending = List.filter ((/=) content) pending }
                    else
                        chat_

                otherEvents ->
                    chat_
    in
        { chat | history = history ++ [ into event ] }


submit : Api.ChatContent -> Chat -> Chat
submit content chat =
    { chat
        | pending = chat.pending ++ [ content ]
        , inputContent = ""
    }


update : Msg -> Chat -> ( Chat, ChatCmd )
update msg ({ inputContent } as chat) =
    case msg of
        Event event ->
            ( updateHistory event chat, UpdateScroll )

        UpdateInput newText ->
            ( { chat | inputContent = newText }, DoNothing )

        SubmitInput ->
            Api.validChatContent inputContent
              |> Maybe.map (\content -> (submit content chat, Send content))
              |> Maybe.withDefault ( chat, DoNothing )


view : Chat -> Html Msg
view { user, history, pending, inputContent } =
    let
        inputField =
            input
                [ placeholder "Input guess here"
                , autofocus True
                , value inputContent
                , onEnter SubmitInput
                , onInput UpdateInput
                ]
                [ text inputContent ]

        ghostsPending =
            List.map (\x -> Ghost { content = Api.showChatContent x }) pending

        viewHistory =
            List.map
                (messageView (Api.showName user))
                (history ++ ghostsPending)
    in
        div [ id "chat" ]
            [ div [ id "messages" ] viewHistory
            , div [ id "chat-input" ] [ inputField ]
            ]
