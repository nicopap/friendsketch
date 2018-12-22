module Main exposing (main)

import Maybe exposing (withDefault)
import Result exposing (Result, toMaybe)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Task exposing (Task)
import Http exposing (Error (Timeout,NetworkError,BadStatus))
import Debug
import API


type Msg
    = CreateGame API.Name
    | OpenGame API.Game API.RoomID API.Name
    | JoinGame API.RoomID API.Name
    | UpdateUserName String
    | UpdateRoom String
    | HttpError Http.Error


type Status
    = NotFailedYet
    | AttemptingConnection
    | InexistantRoom
    | AlreadyTakenName
    | OtherError String


type alias Welcome =
    { username : Result String API.Name
    , roomInput : Result String API.RoomID
    , status : Status
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

validateRoomID : String -> Result String API.RoomID
validateRoomID input =
    case API.validRoomID input of
        Just name ->
            Ok name

        Nothing ->
            Err input


main : Program Never Welcome Msg
main =
    H.program
        { init = ( new, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


new : Welcome
new =
    { username = Err ""
    , roomInput = Err ""
    , status = NotFailedYet
    }



-- UPDATE --


attemptJoin :
    API.Name
    -> API.RoomID
    -> Task Http.Error ( API.Game, API.RoomID, API.Name )
attemptJoin name roomid =
    API.roomsJoinRequest roomid name
        |> Http.toTask
        |> Task.map (\game -> ( game, roomid, name ))


openLink : Result Http.Error ( API.Game, API.RoomID, API.Name ) -> Msg
openLink result =
    case result of
        Ok ( game, roomid, name ) ->
            OpenGame game roomid name

        Err errmsg ->
            HttpError errmsg


{-| Tries to POST /rooms/create { game: "pintclone", settings: settings}
With the answer (the room id), directly attempts to join with given username.
This will redirect the browser to the new page, in case of failure, cry?
-}
attemptCreate : API.Name -> Cmd Msg
attemptCreate username =
    API.roomsCreateRequest API.Pintclone username
        |> Http.toTask
        |> Task.andThen (attemptJoin username)
        |> Task.attempt openLink


{-| Modify model based on the content of the Http response.
-}
processAnswer : Http.Error -> Status
processAnswer error =
    case error of
        Timeout -> OtherError "Request timed out"
        NetworkError -> OtherError "Cannot connect to server"
        (BadStatus { status }) ->
            case status.code of
                404 -> InexistantRoom
                409 -> AlreadyTakenName
                _ -> OtherError status.message
        anyelse -> OtherError (toString anyelse)


update : Msg -> Welcome -> ( Welcome, Cmd Msg )
update msg welcome =
    case msg of
        CreateGame username ->
            ( welcome
            , attemptCreate username
            )

        JoinGame roomid username ->
            ( { welcome | status = AttemptingConnection }
            , Task.attempt openLink <| attemptJoin username roomid
            )

        UpdateUserName newUserName ->
            ( { welcome | username = validateNameInput newUserName }
            , Cmd.none
            )

        UpdateRoom newRoom ->
            ( { welcome | roomInput = validateRoomID newRoom }
            , Cmd.none
            )


        OpenGame game roomid username ->
            ( welcome
            , API.exitToGame game roomid username
            )

        HttpError error ->
            ( { welcome | status = processAnswer error }
            , Debug.log (toString error) Cmd.none
            )



-- VIEW --



hbutton : Maybe Msg -> String -> Html Msg
hbutton maybemsg buttonLabel =
    let
        button attrib =
            H.p [] [H.button attrib [H.text buttonLabel ] ]
    in
        maybemsg
            |> Maybe.map (\msg -> [onClick msg])
            |> Maybe.withDefault [HA.disabled True]
            |> button


type FieldStatus
    = Disabled
    | Bad
    | Neutral


inputField : String -> (String -> Msg) -> FieldStatus -> String -> Html Msg
inputField label msg status content =
    let
        basicTextfield extraAttributes =
            H.p []
                [ H.label [] [ H.text label ]
                , H.input
                    (HA.value content :: extraAttributes)
                    [ H.text content ]
                ]
    in
        basicTextfield <| case status of
            Disabled -> [ HA.disabled True ]
            Bad -> [ HA.class "badinput", onInput msg ]
            Neutral -> [ onInput msg ]


view : Welcome -> Html Msg
view { username, roomInput, status } =
    let
        nameValue =
            case username of
                Err value -> value
                Ok value -> API.showName value

        roomValue =
            case roomInput of
                Err value -> value
                Ok value -> API.showRoomID value

        page {roomStatus, nameStatus} =
            [ inputField "Your username:" UpdateUserName nameStatus nameValue
            , hbutton
                (Maybe.map CreateGame <| toMaybe username)
                "Create a new game room"
            , H.div
                [HA.class "joinbox"]
                [ hbutton
                (Maybe.map2 JoinGame (toMaybe roomInput) (toMaybe username))
                    "Join an existing room"
                , inputField "Room name:" UpdateRoom roomStatus roomValue
                ]
            ]

        info extraClass message =
            [ H.div [HA.class ("info" ++ extraClass)] [H.text message]]
        form =
            case status of
                AlreadyTakenName ->
                    page {roomStatus = Disabled, nameStatus = Bad}
                    ++ info " info-warning" "Name is aleady taken"

                AttemptingConnection ->
                    page {roomStatus = Disabled, nameStatus = Disabled}
                    ++ info "" "Connecting ..."

                NotFailedYet ->
                    page {roomStatus = Neutral, nameStatus = Neutral}

                InexistantRoom ->
                    page {roomStatus = Bad, nameStatus = Neutral}
                    ++ info " info-warning" "That room doesn't exist!"

                OtherError errmsg ->
                    page {roomStatus = Neutral, nameStatus = Neutral}
                    ++ info " info-warning" ("Error: " ++ errmsg)
    in
        H.div [] form
