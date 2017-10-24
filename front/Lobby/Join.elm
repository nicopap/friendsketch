module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http exposing (Error(..))
import Result exposing (Result)
import Maybe
import API


type Msg
    = InputRoom String
    | InputName String
    | HttpAnswer API.RoomID API.Name (Result Http.Error API.Game)


type Status
    = NotFailedYet
    | ConnectingTo API.Game
    | AttemptingConnection
    | InexistantRoom
    | AlreadyTakenName
    | OtherError String


type alias Join =
    { username : Result String API.Name
    , roominput : String
    , status : Status
    }


type alias Flags =
    { username : Maybe String
    }


main : Program Flags Join Msg
main =
    H.programWithFlags
        { init = \x -> ( new x, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


{-| Validate the input and distinguish valid from invalid inputs.
TODO: feedback on why a name is invlaid
-}
validateNameInput : String -> Result String API.Name
validateNameInput input =
    case API.validName input of
        Just name ->
            Ok name

        Nothing ->
            Err input


new : Flags -> Join
new { username } =
    { username = validateNameInput <| Maybe.withDefault "" username
    , roominput = ""
    , status = NotFailedYet
    }


{-| Modify model based on the content of the Http response.
defaultErrorHandle message =
-}
processAnswer : Result Http.Error API.Game -> Status
processAnswer result =
    case result of
        Err Timeout ->
            OtherError "Request timed out"

        Err NetworkError ->
            OtherError "Cannot connect to server (are you offline?)"

        Err (BadStatus { status }) ->
            case status.code of
                404 ->
                    InexistantRoom

                409 ->
                    AlreadyTakenName

                _ ->
                    OtherError status.message

        Err _ ->
            OtherError "misc error"

        Ok game ->
            ConnectingTo game


update : Msg -> Join -> ( Join, Cmd Msg )
update msg model =
    case msg of
        InputRoom roominput ->
            case model.username of
                Ok name ->
                    case API.validRoomID roominput of
                        Nothing ->
                            { model | roominput = roominput } ! []

                        Just validRoom ->
                            { model
                                | status = AttemptingConnection
                                , roominput = roominput
                            }
                                ! [ API.roomsJoinRequest validRoom name
                                        |> Http.send (HttpAnswer validRoom name)
                                  ]

                Err _ ->
                    { model | roominput = roominput } ! []

        InputName username ->
            { model | username = validateNameInput username } ! []

        HttpAnswer validroomid validname response ->
            let
                answer =
                    processAnswer response
            in
                { model | status = answer }
                    ! case answer of
                        ConnectingTo game ->
                            [ API.exitToGame game validroomid validname ]

                        _ ->
                            []


statusView : Status -> Html Msg
statusView status =
    let
        statusTextValue =
            case status of
                OtherError s ->
                    s

                anyelse ->
                    toString anyelse
    in
        H.div [] [ H.text statusTextValue ]


roomFieldLocked : Status -> Bool
roomFieldLocked status =
    case status of
        AlreadyTakenName -> True
        ConnectingTo _ -> True
        AttemptingConnection -> True
        NotFailedYet -> False
        InexistantRoom -> False
        OtherError _ -> False


inputField : String -> (String -> Msg) -> Bool -> String -> Html Msg
inputField label msg disabled content =
    H.p []
        [ H.label [] [ H.text label ]
        , H.input
            [ HA.disabled disabled
            , HA.value content
            , HE.onInput msg
            ]
            [ H.text content ]
        ]


view : Join -> Html Msg
view { username, roominput, status } =
    let
        roomInputView =
            inputField
                ("room name")
                (InputRoom)
                (roomFieldLocked status)
                (roominput)

        nameInput =
            case username of
                Err value ->
                    inputField "Your username" InputName False value

                Ok value ->
                    inputField "Your username"
                        InputName
                        (status /= AlreadyTakenName)
                        (API.showName value)
    in
        H.div []
            [ H.h1 [] [ H.text "Join" ]
            , roomInputView
            , nameInput
            , statusView status
            ]
