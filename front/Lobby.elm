module Main exposing (main)

import Random as Rand
import Maybe exposing (withDefault)
import Array
import Result exposing (Result)
import String
import Char exposing (toUpper)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Task exposing (Task)
import Http
import Debug
import Lobby.NameLists exposing (nameList, adjectiveList)
import API


type Msg
    = CreateGame API.Name Settings
    | OpenGame API.Game API.RoomID API.Name
    | JoinGame API.Name
    | UpdateUserName String
    | ErrorNotify String


type alias Settings =
    { game : API.Game
    }


type alias Welcome =
    { username : Result String API.Name
    , settings : Settings
    , settingsVisible : Bool
    }


{-| Returns the combination of AdjName where, for

    composeName (adjIndx, nameIndx)

The adjective is located at index adjIndx in adjectiveList and the name
is located at index nameIndx in nameList.

-}
composeName : ( Int, Int ) -> String
composeName ( adjIndx, nameIndx ) =
    let
        adjective =
            withDefault "intolerent" <| Array.get adjIndx adjectiveList

        name =
            withDefault "marmoset" <| Array.get nameIndx nameList

        titilize s =
            case String.uncons s of
                Nothing ->
                    ""

                Just ( h, t ) ->
                    String.cons (toUpper h) t
    in
        (titilize adjective) ++ (titilize name)


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


main : Program Never Welcome Msg
main =
    let
        genrand =
            Rand.pair
                (Rand.int 0 <| (Array.length adjectiveList) - 1)
                (Rand.int 0 <| (Array.length nameList) - 1)
                |> Rand.map composeName
    in
        H.program
            { init = ( new, Rand.generate UpdateUserName genrand )
            , update = update
            , view = view
            , subscriptions = always Sub.none
            }


new : Welcome
new =
    { username = Err ""
    , settings = { game = API.Pintclone }
    , settingsVisible = False
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
            ErrorNotify (toString errmsg)


{-| Tries to POST /rooms/create { game: "pintclone", settings: settings}
With the answer (the room id), directly attempts to join with given username.
This will redirect the browser to the new page, in case of failure, cry?
-}
attemptCreate : API.Name -> Settings -> Cmd Msg
attemptCreate username { game } =
    API.roomsCreateRequest game
        |> Http.toTask
        |> Task.andThen (attemptJoin username)
        |> Task.attempt openLink


update : Msg -> Welcome -> ( Welcome, Cmd Msg )
update msg welcome =
    case msg of
        CreateGame username settings ->
            welcome ! [ attemptCreate username settings ]

        JoinGame name ->
            welcome ! [ API.exitToJoin name ]

        UpdateUserName newUserName ->
            { welcome | username = validateNameInput newUserName } ! []

        OpenGame game roomid username ->
            welcome ! [ API.exitToGame game roomid username ]

        ErrorNotify errmsg ->
            Debug.log errmsg welcome ! []



-- VIEW --


settingsView : Settings -> Html msg
settingsView _ =
    H.div [] [ H.text "TODO: work in progress" ]


hbutton : Maybe Msg -> String -> Html Msg
hbutton maybemsg buttonLabel =
    case maybemsg of
        Just msg ->
            H.p [] [ H.button [ onClick msg ] [ H.text buttonLabel ] ]

        Nothing ->
            H.p [] [ H.button [ HA.disabled True ] [ H.text buttonLabel ] ]


submitView : Settings -> Result String API.Name -> List (Html Msg)
submitView settings username =
    case username of
        Ok apiname ->
            [ H.input
                [ HA.autofocus True
                , HA.value <| API.showName apiname
                , onInput UpdateUserName
                , HA.class "goodinput"
                ]
                [ H.text <| API.showName apiname ]
            , hbutton (Just <| CreateGame apiname settings) "Create a new game"
            , hbutton (Just <| JoinGame apiname) "Join an existing game"
            ]

        Err badname ->
            [ H.input
                [ HA.autofocus True
                , HA.value badname
                , onInput UpdateUserName
                , HA.class "badinput"
                ]
                [ H.text badname ]
            , hbutton Nothing "Create a new game"
            , hbutton Nothing "Join an existing game"
            ]


view : Welcome -> Html Msg
view { username, settings, settingsVisible } =
    H.div []
        (H.h1 [] [ H.text "Lobby: What to do?" ]
            :: submitView settings username
            ++ if settingsVisible then
                [ settingsView settings ]
               else
                []
        )
