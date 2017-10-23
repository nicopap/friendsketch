module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Tuple exposing (mapSecond, mapFirst)
import Maybe
import Debug
import Html as H exposing (Html, programWithFlags)
import Pintclone.Room as Room exposing (Room)
import Art.Canvas as Canvas exposing (Canvas)
import API


type GameLobby
    = Uninit
    | LobbyState { room : Room, canvas : Canvas }
    | RoundState { room : Room, canvas : Canvas }
    | FinalState { room : Room, scores : List ( API.Name, Int ) }


type alias Pintclone =
    { state : GameLobby
    , roomid : API.RoomID
    , username : API.Name
    , wslisten : Sub Msg
    , wssend : API.InfoRequest -> Cmd Msg
    }


type alias Flags =
    { roomid : String
    , username : String
    }


type Msg
    = Info (Result String API.InfoMsg)
    | CanvasMsg Canvas.Msg


newCanvas : API.RoomID -> API.Name -> Canvas.State -> Canvas
newCanvas =
    Canvas.new API.Pintclone


main : Program Flags Pintclone Msg
main =
    programWithFlags
        { init = uncurry new << sanitizeFlags
        , update = update
        , view = view
        , subscriptions = subs
        }


{-| Crash if the flags are invalid (because it becomes impossible to
construct a logical program at this point).
-}
sanitizeFlags : Flags -> ( API.RoomID, API.Name )
sanitizeFlags { roomid, username } =
    let
        maybeSanitized =
            Maybe.map2 (,) (API.validRoomID roomid) (API.validName username)

        crashMessage =
            """
            Game initialization failed, please
            check if your browser supports sessionStorage:

                http://caniuse.com/#search=sessionStorage

            If not, please come back with a compatible browser :)"""
    in
        case maybeSanitized of
            Nothing ->
                Debug.crash crashMessage

            Just ret ->
                ret


new : API.RoomID -> API.Name -> ( Pintclone, Cmd msg )
new roomid username =
    ( { state = Uninit
      , roomid = roomid
      , username = username
      , wslisten =
            API.wsinfoListen API.Pintclone roomid username <| Just Info
      , wssend = Cmd.map Info << API.wsinfoSend API.Pintclone roomid username
      }
    , API.wsinfoSend API.Pintclone roomid username API.ReqSync
    )


newScores : API.ScoresState -> Pintclone -> Pintclone
newScores _ pintclone =
    { pintclone | state = Uninit }


{-| Start at the Pregame mode.
-}
newLobby : API.LobbyState -> Pintclone -> Pintclone
newLobby { opponents, master } ({ roomid, username } as pintclone) =
    { pintclone
        | state =
            LobbyState
                { room = Room.newLobby username opponents
                , canvas = newCanvas roomid username Canvas.Pregame
                }
    }


{-| Start a new round of Pintclone.
-}
newRound : API.RoundState -> Pintclone -> Pintclone
newRound { spectators, artist } ({ roomid, username } as pintclone) =
    { pintclone
        | state =
            RoundState <|
                if username == artist then
                    { room = Room.newRound username spectators Room.Me
                    , canvas = newCanvas roomid username Canvas.Artist
                    }
                else
                    { room =
                        Room.newRound username spectators <|
                            Room.Another artist
                    , canvas = newCanvas roomid username Canvas.Spectator
                    }
    }


canvasUpdate : Canvas.Msg -> GameLobby -> ( GameLobby, Cmd Msg )
canvasUpdate msg state =
    let
        newState :
            { room : Room, canvas : Canvas }
            -> ( { room : Room, canvas : Canvas }, Cmd Msg )
        newState { room, canvas } =
            let
                ( newCanvas, response ) =
                    Canvas.update msg canvas
            in
                { room = room, canvas = newCanvas }
                    ! [ Cmd.map CanvasMsg response ]
    in
        case state of
            Uninit ->
                Uninit ! []

            LobbyState state_ ->
                mapFirst LobbyState <| newState state_

            RoundState state_ ->
                mapFirst RoundState <| newState state_

            FinalState state_ ->
                FinalState state_ ! []


roomChange : i -> GameLobby -> (i -> Room -> Room) -> Cmd m -> ( GameLobby, Cmd m )
roomChange name state change ifError =
    case state of
        Uninit ->
            state ! [ ifError ]

        LobbyState { room, canvas } ->
            LobbyState { room = change name room, canvas = canvas } ! []

        RoundState { room, canvas } ->
            RoundState { room = change name room, canvas = canvas } ! []

        FinalState { room, scores } ->
            FinalState { room = change name room, scores = scores } ! []


updateInfo : API.InfoMsg -> Pintclone -> ( Pintclone, Cmd Msg )
updateInfo msg ({ username, state, wssend } as pintclone) =
    case msg of
        API.Joined name ->
            roomChange name state Room.joins (wssend API.ReqSync)
                |> mapFirst (\newstate -> { pintclone | state = newstate })

        API.Left name ->
            roomChange name state Room.leaves (wssend API.ReqSync)
                |> mapFirst (\newstate -> { pintclone | state = newstate })

        API.Sync gamestate ->
            case gamestate of
                API.Summary scores ->
                    newScores scores pintclone ! []

                API.Round round ->
                    newRound round pintclone ! []

                API.Lobby lobby ->
                    newLobby lobby pintclone ! []

        API.Mastery ->
            roomChange never state (always Room.becomeMaster) (wssend API.ReqSync)
                |> mapFirst (\newstate -> { pintclone | state = newstate })


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg pintclone =
    case msg of
        Info (Ok msg_) ->
            updateInfo msg_ pintclone

        Info (Err error) ->
            Debug.log ("A decoder error happend: " ++ error)
                ( pintclone, Cmd.none )

        CanvasMsg msg_ ->
            mapFirst (\x -> { pintclone | state = x })
                (canvasUpdate msg_ pintclone.state)


subs : Pintclone -> Sub Msg
subs { wslisten } =
    wslisten


view : Pintclone -> Html Msg
view pintclone =
    case pintclone.state of
        Uninit ->
            H.div [] [ H.h1 [] [ H.text "Loading ..." ] ]

        LobbyState { room, canvas } ->
            let
                -- TODO: add button to start the game
                waitText =
                    if Room.isMaster room then
                        H.h1 []
                            [ H.b [] [ H.text "YOU ARE THE ROOM MASTER" ] ]
                    else
                        H.h1 [] [ H.text "Waiting on room master to start" ]
            in
                H.div []
                    [ waitText
                    , H.p []
                        [ Room.view room
                        , H.map CanvasMsg <| Canvas.view canvas
                        ]
                    ]

        RoundState { room, canvas } ->
            H.div []
                [ H.p []
                    [ Room.view room
                    , H.map CanvasMsg <| Canvas.view canvas
                    ]
                ]

        FinalState { room, scores } ->
            H.div [] [ H.h1 [] [ H.text "NOT IMPLEMENTED YET" ] ]
