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

        Err anyelse ->
            OtherError <| toString anyelse

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


type FieldStatus
    = Disabled
    | Bad String
    | Neutral


inputField : String -> (String -> Msg) -> FieldStatus -> String -> Html Msg
inputField label msg status content =
    let
        basicButton extraAttributes extraLabel =
            H.p []
                ([ H.label [] [ H.text label ]
                 , H.input (HA.value content :: extraAttributes) [ H.text content ]
                 ]
                    ++ extraLabel
                )
    in
        case status of
            Disabled ->
                basicButton [ HA.disabled True ] []

            Bad errmsg ->
                basicButton
                    [ HA.class "badinput", HE.onInput msg ]
                    [ H.text errmsg ]

            Neutral ->
                basicButton [ HE.onInput msg ] []


view : Join -> Html Msg
view { username, roominput, status } =
    let
        ( nameValue, nameErr ) =
            case username of
                Err value ->
                    ( value, "Invalid :/" )

                Ok value ->
                    ( API.showName value, "" )

        form =
            case status of
                AlreadyTakenName ->
                    [ inputField "Room:" InputRoom Disabled roominput
                    , inputField "Your name:"
                        InputName
                        (Bad <| nameErr ++ "already taken")
                        nameValue
                    , H.div [ HA.class "info info-warning" ]
                        [ H.text "Name is aleady taken" ]
                    ]

                ConnectingTo _ ->
                    [ inputField "Room:" InputRoom Disabled roominput
                    , inputField "Your name:" InputName Disabled nameValue
                    , H.div [ HA.class "info" ] [ H.text "Connecting ..." ]
                    ]

                AttemptingConnection ->
                    [ inputField "Room:" InputRoom Disabled roominput
                    , inputField "Your name:" InputName Disabled nameValue
                    , H.div [ HA.class "info" ] [ H.text "Connecting ..." ]
                    ]

                NotFailedYet ->
                    [ inputField "Room:" InputRoom Neutral roominput
                    , inputField "Your name:" InputName Disabled nameValue
                    ]

                InexistantRoom ->
                    [ inputField "Room:" InputRoom (Bad "") roominput
                    , inputField "Your name:" InputName Disabled nameValue
                    , H.div [ HA.class "info info-warning" ]
                        [ H.text "That room doesn't exist!" ]
                    ]

                OtherError errmsg ->
                    [ inputField "Room:" InputRoom Neutral roominput
                    , inputField "Your name:" InputName Neutral nameValue
                    , H.div [ HA.class "info info-warning" ]
                        [ H.text ("Error: " ++ errmsg) ]
                    ]
    in
        H.div [ HA.id "lobbydiv" ]
            (H.h1 [] [ H.text "Join" ] :: form)
