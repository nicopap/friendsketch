module Lobby.Join exposing (view, update, new, subs, Msg, Join)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http exposing (Error(..))
import Result exposing (Result)
import Json.Decode as JDec exposing (Decoder)
import Dict
import Room exposing (Room)


type Msg
    = TypeRoomLetter String
    | TypeNameLetter String
    | HttpAnswer (Result Http.Error String)
    | Nop


type Status
    = NotFailedYet
    | AttemptingConnection
    | InexistantRoom
    | AlreadyTakenName
    | Established Room
    | OtherError String


type alias Join =
    { userName : String
    , roomId : String
    , status : Status
    , baseurl : String
    }


new : String -> String -> Join
new baseurl userName =
    { userName = userName
    , roomId = ""
    , status = NotFailedYet
    , baseurl = baseurl
    }


canBeRoom : String -> Join -> ( Join, Cmd Msg )
canBeRoom roomId ({ status, userName } as model) =
    if String.length roomId == 10 && status /= AttemptingConnection then
        ( { model | status = AttemptingConnection, roomId = roomId }
        , Http.send HttpAnswer <|
            Http.post
                (Room.join roomId)
                (Http.stringBody "text/plain" userName)
                (JDec.string)
        )
    else
        ( { model | roomId = roomId }, Cmd.none )


{-| Modify model based on the content of the Http response.
-}
processAnswer : Result Http.Error String -> Join -> Join
processAnswer result ({ baseurl, userName } as model) =
    let
        defaultErrorHandle message =
            { model | status = OtherError <| "Internal error" ++ message }

        successHandle headerfields =
            case Dict.get "location" headerfields of
                Just infoWSloc ->
                    { model
                        | status =
                            Established <|
                                Room.new baseurl infoWSloc userName
                    }

                Nothing ->
                    defaultErrorHandle "No location field on 303 status"
    in
        case result of
            Err (BadUrl message) ->
                defaultErrorHandle <| "Bad url: " ++ message

            Err Timeout ->
                defaultErrorHandle "Request timed out"

            Err NetworkError ->
                defaultErrorHandle
                    "Cannot connect to server (maybe you are offline)"

            Err (BadStatus { status, headers }) ->
                case status.code of
                    404 ->
                        { model | status = InexistantRoom }

                    409 ->
                        { model | status = AlreadyTakenName }

                    303 ->
                        successHandle headers

                    _ ->
                        defaultErrorHandle <| "Bad status" ++ status.message

            Err (BadPayload errmsg _) ->
                defaultErrorHandle <| "Bad Payload" ++ errmsg

            Ok _ ->
                defaultErrorHandle "Bad Status"


update : Join -> Msg -> ( Join, Cmd Msg )
update model msg =
    case msg of
        TypeRoomLetter roomId ->
            canBeRoom roomId model

        TypeNameLetter userName ->
            { model | userName = userName } ! []

        HttpAnswer response ->
            processAnswer response model ! []

        Nop ->
            model ! []


subs : Join -> Sub Msg
subs { status } =
    case status of
        Established room ->
            Room.infoSub (always Nop) room

        _ ->
            Sub.none


statusView : Status -> Html Msg
statusView status =
    case status of
        NotFailedYet ->
            H.div [] [ H.text "NotFailedYet" ]

        InexistantRoom ->
            H.div [] [ H.text "InexistantRoom" ]

        AlreadyTakenName ->
            H.div [] [ H.text "AlreadyTakenName" ]

        Established room ->
            Room.view room

        AttemptingConnection ->
            H.div [] [ H.text "AttemptingConnection" ]

        OtherError s ->
            H.div [] [ H.text <| "OtherError " ++ s ]


roomFieldLocked : Status -> Bool
roomFieldLocked status =
    case status of
        AlreadyTakenName -> True
        Established _ -> True
        AttemptingConnection -> True
        NotFailedYet -> False
        InexistantRoom -> False
        OtherError _ -> False


view : Join -> Html Msg
view { userName, roomId, status } =
    let
        roomInput =
            H.p []
                [ H.label [] [ H.text "room name" ]
                , H.input
                    (if roomFieldLocked status then
                        [ HA.autofocus True, HE.onInput TypeRoomLetter ]
                     else
                        [ HA.disabled True ]
                    )
                    [ H.text roomId ]
                ]

        nameInput =
            H.p []
                [ H.label [] [ H.text "Your user name" ]
                , H.input
                    (if status /= AlreadyTakenName then
                        [ HA.disabled True, HA.value userName ]
                     else
                        [ HE.onInput TypeNameLetter, HA.value userName ]
                    )
                    [ H.text userName ]
                ]
    in
        H.div []
            [ H.h1 [] [ H.text "Join" ]
            , roomInput
            , nameInput
            , statusView status
            ]
