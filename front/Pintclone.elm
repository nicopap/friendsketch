module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Tuple exposing (mapSecond, mapFirst)
import Maybe
import Debug
import Html as H exposing (Html, programWithFlags)
import Html.Attributes as HA
import Html.Events as HE
import Pintclone.Room as Room exposing (Room)
import Canvas exposing (Canvas)
import Ports
import API


type alias LobbyState_T =
    { room : Room
    , canvas : Canvas
    , passHidden : Bool
    }


type GameLobby
    = Uninit
    | LobbyState LobbyState_T
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


type LobbyMsg
    = TogglePassView
    | StartGame


type Msg
    = Info (Result String API.InfoMsg)
    | LobbyMsg LobbyMsg
    | CanvasMsg Canvas.Msg


roomiddisplay : String
roomiddisplay =
    "roomiddisplay"


copyCatch : API.RoomID -> Cmd msg
copyCatch roomid =
    Ports.copyCatch ( roomiddisplay, API.showRoomID roomid )


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
newLobby : API.LobbyState -> Pintclone -> ( Pintclone, Cmd msg )
newLobby { opponents, master } ({ roomid, username } as pintclone) =
    let
        status =
            if master then
                Room.Master
            else
                Room.Peasant
    in
        { pintclone
            | state =
                LobbyState
                    { room = Room.newLobby status username opponents
                    , canvas = newCanvas roomid username Canvas.Pregame
                    , passHidden = True
                    }
        }
            ! if master then
                [ copyCatch roomid ]
              else
                []


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
            { a | room : Room, canvas : Canvas }
            -> ( { a | room : Room, canvas : Canvas }, Cmd Msg )
        newState ({ room, canvas } as state_) =
            let
                ( newCanvas, response ) =
                    Canvas.update msg canvas
            in
                { state_ | room = room, canvas = newCanvas }
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

        LobbyState ({ room } as state_) ->
            LobbyState { state_ | room = change name room } ! []

        RoundState ({ room } as state_) ->
            RoundState { state_ | room = change name room } ! []

        FinalState ({ room } as state_) ->
            FinalState { state_ | room = change name room } ! []


updateInfo : API.InfoMsg -> Pintclone -> ( Pintclone, Cmd Msg )
updateInfo msg ({ username, state, wssend } as pintclone) =
    let
        mutateRoom name action =
            roomChange name state action (wssend API.ReqSync)
                |> mapFirst (\newstate -> { pintclone | state = newstate })
    in
        case msg of
            API.Joined name ->
                mutateRoom name Room.joins

            API.Left name ->
                mutateRoom name Room.leaves

            API.Sync gamestate ->
                case gamestate of
                    API.Summary scores ->
                        newScores scores pintclone ! []

                    API.Round round ->
                        newRound round pintclone ! []

                    API.Lobby lobby ->
                        newLobby lobby pintclone

            API.Mastery ->
                mutateRoom never (always Room.becomeMaster)
                    |> mapSecond
                        (\batch ->
                            Cmd.batch [ copyCatch pintclone.roomid, batch ]
                        )


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg pintclone =
    case ( msg, pintclone.state ) of
        ( Info (Ok msg_), _ ) ->
            updateInfo msg_ pintclone

        ( Info (Err error), _ ) ->
            Debug.log ("A decoder error happend: " ++ error)
                ( pintclone, Cmd.none )

        ( CanvasMsg msg_, state ) ->
            mapFirst (\x -> { pintclone | state = x })
                (canvasUpdate msg_ state)

        ( LobbyMsg TogglePassView, LobbyState lobby ) ->
            { pintclone
                | state =
                    LobbyState { lobby | passHidden = not lobby.passHidden }
            }
                ! []

        ( LobbyMsg StartGame, LobbyState _ ) ->
            pintclone ! [ pintclone.wssend API.ReqStart ]

        ( anymsg, anystate ) ->
            Debug.log
                ("An inconsistent message happend: "
                    ++ toString anymsg
                    ++ "||"
                    ++ toString anystate
                )
                pintclone
                ! [ pintclone.wssend API.ReqSync ]


subs : Pintclone -> Sub Msg
subs { wslisten } =
    wslisten


lobbyView : LobbyState_T -> API.RoomID -> Html Msg
lobbyView { room, canvas, passHidden } roomid =
    let
        hiddenStateName =
            if passHidden then
                "password"
            else
                "input"

        masterDialog =
            H.div []
                [ H.label []
                    [ H.text "Room name: "
                    , H.input
                        [ HA.type_ hiddenStateName
                        , HA.readonly True
                        , HA.value <| API.showRoomID roomid
                        , HA.selected True
                        , HA.autofocus True
                        , HA.size 6
                        , HA.id roomiddisplay
                        ]
                        []
                    , H.input
                        [ HA.type_ "checkbox"
                        , HE.onClick <| LobbyMsg TogglePassView
                        ]
                        []
                    ]
                , H.p []
                    [ H.button
                        [ HE.onClick <| LobbyMsg StartGame ]
                        [ H.text "Start the game!" ]
                    ]
                ]

        isMaster =
            Room.isMaster room

        waitText =
            if isMaster then
                H.h1 []
                    [ H.b [] [ H.text "YOU ARE THE ROOM MASTER" ] ]
            else
                H.h1 [] [ H.text "Waiting on room master to start" ]
    in
        H.div []
            [ waitText
            , H.p []
                [ Room.view room
                , if isMaster then
                    masterDialog
                  else
                    H.p [] []
                , H.map CanvasMsg <| Canvas.view canvas
                ]
            ]


view : Pintclone -> Html Msg
view pintclone =
    case pintclone.state of
        Uninit ->
            H.div [] [ H.h1 [] [ H.text "Loading ..." ] ]

        LobbyState state ->
            lobbyView state pintclone.roomid

        RoundState { room, canvas } ->
            H.div []
                [ H.p []
                    [ Room.view room
                    , H.map CanvasMsg <| Canvas.view canvas
                    ]
                ]

        FinalState { room, scores } ->
            H.div [] [ H.h1 [] [ H.text "NOT IMPLEMENTED YET" ] ]
