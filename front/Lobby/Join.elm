module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http exposing (Error(..))
import Result exposing (Result)
import Tuple exposing (mapFirst)
import Maybe
import API
import Ports exposing (openLink)


type Msg
    = InputRoom String
    | InputName String
    | HttpAnswer (Result Http.Error API.Game)


type Status
    = NotFailedYet
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
        { init = \x -> (new x, Cmd.none)
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


attemptRoom : String -> API.Name -> Maybe (Cmd Msg)
attemptRoom roominput name =
    Maybe.map
        (\v -> Http.send HttpAnswer <| API.roomsJoinRequest v name)
        (API.validRoomID roominput)


{-| Modify model based on the content of the Http response.
-}
processAnswer : Result Http.Error API.Game -> ( Status, Cmd Msg )
processAnswer result =
    let
        defaultErrorHandle message =
            ( OtherError ("Internal error:" ++ message), Cmd.none )

        successHandle game =
            ( AttemptingConnection, openLink <| API.gamepage game )
    in
        case result of
            Err Timeout ->
                defaultErrorHandle "Request timed out"

            Err NetworkError ->
                defaultErrorHandle "Cannot connect to server (offline?)"

            Err (BadStatus { status }) ->
                case status.code of
                    404 -> InexistantRoom ! []
                    409 -> AlreadyTakenName ! []
                    _ -> defaultErrorHandle status.message

            Err _ ->
                defaultErrorHandle "misc error"

            Ok game ->
                successHandle game


update : Msg -> Join -> ( Join, Cmd Msg )
update msg model =
    case msg of
        InputRoom roominput ->
            case model.username of
                Ok name ->
                    case attemptRoom roominput name of
                        Just cmd ->
                            ( { model
                                | status = AttemptingConnection
                                , roominput = roominput
                              }
                            , cmd
                            )

                        Nothing ->
                            { model | roominput = roominput } ! []

                Err _ ->
                    { model | roominput = roominput } ! []

        InputName username ->
            { model | username = validateNameInput username } ! []

        HttpAnswer response ->
            mapFirst
                (\x -> { model | status = x })
                (processAnswer response)


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
        AttemptingConnection -> True
        NotFailedYet -> False
        InexistantRoom -> False
        OtherError _ -> False

inputField : String -> (String -> Msg) -> Bool -> String -> Html Msg
inputField label msg disabled content =
    H.p []
        [ H.label [] [ H.text label ]
        , H.input [ HA.disabled disabled, HE.onInput msg ] [ H.text content ]
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
