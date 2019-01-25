module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Tuple exposing (mapSecond, mapFirst)
import Maybe
import Debug
import Html as H exposing (Html, programWithFlags, div, p, b, h1, h3, text, pre)
import Html.Attributes as HA exposing (id, class, href)
import Html.Events as HE
import Pintclone.Room as Room exposing (Room)
import Canvas exposing (Canvas)
import Process
import Task
import Time exposing (second)
import Ports
import API exposing (GameReq(InfoReq, CanvasReq), GameMsg)


type alias LobbyState_T =
    { room : Room
    , canvas : Canvas
    , hideId : Bool
    }

type RetryStatus
    = OneAttempt
    | Attempt Int
    | GivenUp Int
    | NoAttempt

type GameLobby
    = Uninit
    | LobbyState LobbyState_T
    | RoundState { room : Room, canvas : Canvas }
    | FinalState { room : Room, scores : List ( API.Name, Int ) }
    | ErrorState { title : String, message : String, retry : RetryStatus }


type alias Pintclone =
    { state : GameLobby
    , roomid : API.RoomID
    , username : API.Name
    , wslisten : Maybe (Result API.ListenError GameMsg -> Msg) -> Sub Msg
    , wssend : API.GameReq -> Cmd Msg
    , syncRetries : Int
    , openGameRetries : Int
    }


type alias Flags =
    { roomid : String
    , username : String
    , retries : Int
    }


type LobbyMsg
    = TogglePassView
    | StartGame


type Msg
    = Info API.InfoMsg
    | LobbyMsg LobbyMsg
    | CanvasMsg Canvas.Msg
    | ListenError API.ListenError
    | Dummy
    | Reopen


copyCatch : API.RoomID -> Cmd msg
copyCatch roomid =
    Ports.copyCatch ( "roomiddisplay", API.showRoomID roomid )



main : Program Flags Pintclone Msg
main =
    programWithFlags
        { init = new << sanitizeFlags
        , update = update
        , view = view
        , subscriptions = subs
        }


{-| Crash if the flags are invalid (because it becomes impossible to
construct a logical program at this point).
-}
sanitizeFlags : Flags -> ( API.RoomID, API.Name, Int )
sanitizeFlags { roomid, username, retries } =
    let
        maybeSanitized =
            Maybe.map3 (,,)
                (API.validRoomID roomid)
                (API.validName username)
                (Just retries)

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


new : (API.RoomID, API.Name, Int) -> ( Pintclone, Cmd Msg )
new (roomid, username, openGameRetries) =
    ( { state = Uninit
      , roomid = roomid
      , username = username
      , wslisten = API.wsListen roomid username
      , wssend = API.wsSend roomid username
      , syncRetries = 0
      , openGameRetries = openGameRetries
      }
    , API.wsSend roomid username (InfoReq API.ReqSync)
    )


newScores : API.ScoresState -> Pintclone -> Pintclone
newScores _ pintclone =
    { pintclone | state = Uninit }


{-| Start at the Pregame mode.
-}
newLobby : API.LobbyState -> Pintclone -> ( Pintclone, Cmd msg )
newLobby { players, master } ({ roomid, username } as pintclone) =
    let
        (status, commands) =
            if master == username then
                (Room.Master, copyCatch roomid)
            else
                (Room.Peasant, Cmd.none)

        newState =
            LobbyState
                { room = Room.newLobby status username players
                , canvas = Canvas.new [] Canvas.Pregame
                , hideId = True
                }
    in
        ( { pintclone | state = newState }, commands )


{-| Start a new round of Pintclone.
-}
newRound : API.Drawing -> API.RoundState -> Pintclone -> Pintclone
newRound drawing { playerScores, artist } ({ username } as pintclone) =
    let
        scores =
            List.map Tuple.first playerScores

        newInnerState room canvasState =
            { room = Room.newRound username scores room
            , canvas = Canvas.new drawing canvasState
            }

        newGameState =
            if username == artist then
                newInnerState Room.Me Canvas.Artist
            else
                newInnerState (Room.Another artist) Canvas.Spectator
    in
        { pintclone | state = RoundState newGameState }


updateCanvas : Canvas.Msg -> GameLobby -> ( GameLobby, Maybe API.CanvasMsg )
updateCanvas msg state =
    let
        newState constr state_ =
            let
                ( newCanvas, response ) =
                    Canvas.update msg state_.canvas
            in
                ( constr { state_ | canvas = newCanvas }, response )
    in
        case state of
            LobbyState state_ ->
                newState LobbyState state_

            RoundState state_ ->
                newState RoundState state_

            anyelse ->
                ( anyelse, Nothing )


roomMod : i -> GameLobby -> (i -> Room -> Room) -> Cmd m -> ( GameLobby, Cmd m )
roomMod name state change ifUninit =
    let
        mod f state_ room =
            ( f { state_ | room = change name room }, Cmd.none )
    in
        case state of
            Uninit ->
                ( state , ifUninit )

            LobbyState ({ room } as state_) ->
                mod LobbyState state_ room

            RoundState ({ room } as state_) ->
                mod RoundState state_ room

            FinalState ({ room } as state_) ->
                mod FinalState state_ room

            ErrorState _ ->
                ( state, Cmd.none )


updateInfo : API.InfoMsg -> Pintclone -> ( Pintclone, Cmd Msg )
updateInfo msg ({ username, state, wssend } as pintclone) =
    let
        mutateRoom name action =
            roomMod name state action (wssend <| InfoReq API.ReqSync)
                |> mapFirst (\newstate -> { pintclone | state = newstate })
    in
        case msg of
            API.Joined name ->
                mutateRoom name Room.joins

            API.Left name ->
                mutateRoom name Room.leaves

            API.Mastery ->
                mutateRoom never (always Room.becomeMaster)
                    |> mapSecond
                        (\b -> Cmd.batch [ copyCatch pintclone.roomid, b ])

            API.Sync gamestate ->
                case gamestate of
                    API.Summary scores ->
                        ( newScores scores pintclone, Cmd.none )

                    API.Round drawing round ->
                        ( newRound drawing round pintclone, Cmd.none )

                    API.Lobby lobby ->
                        newLobby lobby pintclone


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg ({ roomid, username, openGameRetries, syncRetries } as pintclone_) =
    let
        pintclone =
            { pintclone_ | syncRetries = 0 }

        toCmd maybeCanvasMsg =
            case maybeCanvasMsg of
                Just canvasMsg ->
                    pintclone.wssend <| CanvasReq canvasMsg

                Nothing ->
                    Cmd.none

        report error_msg =
            let
                full_msg =
                    "Failure in '" ++ API.showRoomID roomid ++ "' for '"
                    ++ API.showName username ++ "' with the following error: "
                    ++ error_msg
            in
                API.reportError full_msg |> Cmd.map (\() -> Dummy)

        updateToError title message retry =
            let
                newErrorState =
                    ErrorState { message=message, retry=retry, title=title }
            in
                ( { pintclone | state = newErrorState }, report message )
    in
        case ( msg, pintclone.state ) of
            ( Info msg_, _ ) ->
                updateInfo msg_ pintclone

            ( CanvasMsg msg_, state ) ->
                updateCanvas msg_ state
                    |> mapFirst (\x -> { pintclone | state = x })
                    |> mapSecond toCmd

            ( LobbyMsg TogglePassView, LobbyState ({ hideId } as lobby) ) ->
                ( { pintclone
                    | state = LobbyState { lobby | hideId = not hideId }
                  }
                , Cmd.none
                )

            ( LobbyMsg StartGame, LobbyState _ ) ->
                ( pintclone, pintclone.wssend <| InfoReq API.ReqStart )

            ( ListenError API.BadSend, _) ->
                if openGameRetries >= 2 then
                    updateToError
                        "Communication error"
                        ("Attempted to connect " ++ toString openGameRetries
                        ++ " times without success. Giving up")
                        (GivenUp (openGameRetries + 1))
                else
                    let
                        attempts =
                            if openGameRetries == 0 then
                                OneAttempt
                            else
                                Attempt (openGameRetries + 1)
                        newErrorState =
                            ErrorState
                                { message = "Websocket connection error"
                                , title = "Communication error (reconnecting)"
                                , retry = attempts
                                }
                    in
                        ( { pintclone | state = newErrorState }
                        , Process.sleep (2.4 * second)
                            |> Task.perform (always Reopen)
                        )

            ( ListenError (API.DecodeError msg), _) ->
                updateToError "Communication error" msg NoAttempt

            ( Dummy, _ ) ->
                ( pintclone, Cmd.none )

            ( Reopen, _ ) ->
                ( pintclone
                , API.exitToGame
                    API.Pintclone roomid username (openGameRetries + 1)
                )

            ( anymsg, anystate ) ->
                if syncRetries >= 3 then
                    updateToError "Synchronisation error"
                        (toString (anymsg, anystate))
                        NoAttempt
                else
                    Debug.log
                        ("Inconsistency:" ++ toString ( anymsg, anystate ))
                        ( { pintclone | syncRetries = syncRetries + 1 }
                        , pintclone.wssend <| InfoReq API.ReqSync
                        )


subs : Pintclone -> Sub Msg
subs { wslisten, state } =
    let
        listen : Maybe (API.CanvasMsg -> Msg) -> Result API.ListenError GameMsg -> Msg
        listen redirectCanvas response =
            case response of
                Err error ->
                    ListenError error

                Ok (API.InfoMsg msg) ->
                    Info msg

                Ok (API.CanvasMsg msg) ->
                    case redirectCanvas of
                        Just redirect ->
                            redirect msg

                        Nothing ->
                            ListenError <| API.DecodeError "Recieved a canvas message"

        cListen canvas =
            listen <| Maybe.map ((<<) CanvasMsg) <| Canvas.subsAdaptor canvas

        toSend =
            case state of
                LobbyState { canvas } ->
                    cListen canvas

                FinalState _ ->
                    listen Nothing

                RoundState { canvas } ->
                    cListen canvas

                Uninit ->
                    listen Nothing

                ErrorState _ ->
                    listen Nothing
    in
        wslisten <| Just toSend


masterDialog : Bool -> API.RoomID -> Html Msg
masterDialog hideId roomid =
    let
        roomdisplayAttributes =
            [ HA.type_ "text"
            , class (if hideId then "hidden-roomid" else "display-roomid")
            , HA.value <| API.showRoomID roomid
            , HA.attribute "data-autoselect" ""
            , HA.readonly True
            ]
    in
        div [ id "roomiddialog" ]
            [ div []
                [ H.h3 [] [text "Room name"]
                , H.p [] [text "share this with people to let them join your game"]
                , H.input roomdisplayAttributes []
                , H.input
                    [ HA.type_ "checkbox"
                    , HE.onClick <| LobbyMsg TogglePassView
                    ]
                    []
                ]
            , div []
                [ H.button
                    [ HE.onClick <| LobbyMsg StartGame ]
                    [ text "Start the game!" ]
                ]
            ]


view : Pintclone -> Html Msg
view pintclone =
    let
        gameView room canvas =
            div [ id "masterlayout" ]
                [ Room.view room
                , H.map CanvasMsg <| Canvas.view canvas
                ]
    in
        case pintclone.state of
            Uninit ->
                div [] [ h1 [] [ text "Opening game" ] ]

            LobbyState { room, canvas, hideId } ->
                div []
                    [ gameView room canvas
                    , if Room.isMaster room then
                        masterDialog hideId pintclone.roomid
                      else
                        p [] []
                    ]

            RoundState { room, canvas } ->
                gameView room canvas

            FinalState { room, scores } ->
                div [] [ h1 [] [ text "NOT IMPLEMENTED YET" ] ]

            ErrorState { title, message, retry } ->
                let
                    firstParagraph =
                        case retry of
                            GivenUp c ->
                                "After " ++ toString c ++ " tries, we still"
                                ++ " couldn't connect to the game,"
                                ++ " your only option is to try to join a"
                                ++ " different room or change display name."
                            OneAttempt ->
                                "Couldn't connect to the game. Retrying..."
                            Attempt c ->
                                "After " ++ toString c ++ " tries, we still"
                                ++ " couldn't connect to the game. Retrying..."
                            NoAttempt ->
                                "An irreversible error occured and"
                                ++ " your only option is to try to join a"
                                ++ " different room or change display name."
                    secondParagraph =
                        "We are sorry for the inconvenience this causes you."
                    thirdParagraph =
                        "A copy of the following error message has already"
                        ++ " been sent to the developers. We will take care"
                        ++ " that you won't experience this in the future."
                in
                    div []
                        [ h1 [] [ text title ]
                        , p [] [ text firstParagraph ]
                        , H.a
                            [ href "/friendk/lobby/index.html"]
                            [ H.button [] [ text "Join a different game" ] ]
                        , p [] [ text secondParagraph ]
                        , p [] [ text thirdParagraph ]
                        , pre [] [ text message ]
                        ]
