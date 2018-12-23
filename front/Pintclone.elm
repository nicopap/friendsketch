module Main exposing (main)

{-| The top level logic for the Pintclone game. It's router that interprets
room websocket messages and apply changes to the state of modules
accordingly.
-}

import Tuple exposing (mapSecond, mapFirst)
import Maybe
import Debug
import Html as H exposing (Html, programWithFlags, div, p, b, h1, text)
import Html.Attributes as HA exposing (id, class)
import Html.Events as HE
import Pintclone.Room as Room exposing (Room)
import Canvas exposing (Canvas)
import Ports
import API exposing (GameReq(InfoReq, CanvasReq), GameMsg)


type alias LobbyState_T =
    { room : Room
    , canvas : Canvas
    , hideId : Bool
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
    , wslisten : Maybe (Result String GameMsg -> Msg) -> Sub Msg
    , wssend : API.GameReq -> Cmd Msg
    }


type alias Flags =
    { roomid : String
    , username : String
    }


type LobbyMsg
    = TogglePassView
    | StartGame


type Msg
    = Info API.InfoMsg
    | LobbyMsg LobbyMsg
    | CanvasMsg Canvas.Msg
    | ServerError String


copyCatch : API.RoomID -> Cmd msg
copyCatch roomid =
    Ports.copyCatch ( "roomiddisplay", API.showRoomID roomid )


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


new : API.RoomID -> API.Name -> ( Pintclone, Cmd Msg )
new roomid username =
    ( { state = Uninit
      , roomid = roomid
      , username = username
      , wslisten = API.wsListen roomid username
      , wssend = API.wsSend roomid username
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
        status =
            if master then
                Room.Master
            else
                Room.Peasant
    in
        ( { pintclone
            | state =
                LobbyState
                    { room = Room.newLobby status username players
                    , canvas = newCanvas roomid username Canvas.Pregame
                    , hideId = True
                    }
          }
        , if master then
            copyCatch roomid
          else
            Cmd.none
        )


{-| Start a new round of Pintclone.
-}
newRound : API.RoundState -> Pintclone -> Pintclone
newRound { playerScores, artist } ({ roomid, username } as pintclone) =
    let
        scores =
            List.map Tuple.first playerScores

        newState room canvas =
            { room = Room.newRound username scores room
            , canvas = newCanvas roomid username canvas
            }
    in
        { pintclone
            | state =
                RoundState <|
                    if username == artist then
                        newState Room.Me Canvas.Artist
                    else
                        newState (Room.Another artist) Canvas.Spectator
        }


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
roomMod name state change ifError =
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
                        newScores scores pintclone ! []

                    API.Round round ->
                        newRound round pintclone ! []

                    API.Lobby lobby ->
                        newLobby lobby pintclone


update : Msg -> Pintclone -> ( Pintclone, Cmd Msg )
update msg pintclone =
    let
        toCmd maybeCanvasMsg =
            case maybeCanvasMsg of
                Just canvasMsg ->
                    pintclone.wssend <| CanvasReq canvasMsg

                Nothing ->
                    Cmd.none
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

            ( anymsg, anystate ) ->
                Debug.log
                    ("Inconsistent message:" ++ toString (anymsg, anystate))
                    ( pintclone, pintclone.wssend <| InfoReq API.ReqSync )


subs : Pintclone -> Sub Msg
subs { wslisten, state } =
    let
        listen : Maybe (API.CanvasMsg -> Msg) -> Result String GameMsg -> Msg
        listen redirectCanvas response =
            case response of
                Err error ->
                    ServerError error

                Ok (API.InfoMsg msg) ->
                    Info msg

                Ok (API.CanvasMsg msg) ->
                    case redirectCanvas of
                        Just redirect ->
                            redirect msg

                        Nothing ->
                            ServerError "Recieved a canvas message"

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
    in
        wslisten <| Just toSend


masterDialog : Bool -> API.RoomID -> Html Msg
masterDialog hideId roomid =
    let
        hiddenStateName =
            if hideId then
                "password"
            else
                "input"
    in
        div [ id "roomiddialog" ]
            [ div []
                [ text "Room name:"
                , H.input
                    [ HA.type_ hiddenStateName
                    , HA.readonly True
                    , HA.value <| API.showRoomID roomid
                    ]
                    []
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
                div [] [ h1 [] [ text "Loading ..." ] ]

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
