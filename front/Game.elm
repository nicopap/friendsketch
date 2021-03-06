module Game exposing
    ( Game
    , Msg
    , Event
    , GameCmd(..)
    , sync
    , view
    , update
    , receive
    , subs
    )


import Tuple exposing (first, mapFirst, mapSecond)
import Time exposing (every, second)
import Task
import Process exposing (sleep)
import Dict exposing (Dict)

import Html as H exposing (Html, div, p, b, h1, h3, text, pre, input, button, a)
import Html.Attributes as HA exposing (id, class, href, disabled)
import Html.Events as HE exposing (onClick)

import Api
import Ports
import Game.Room as Room exposing (Room)
import Game.Guess as Guess exposing (Guess)
import Canvas exposing (Canvas)
import Chat exposing (Chat)


type GamePart
    = LobbyState { hideId : Bool }
    | Round Guess
    | BetweenRound Guess
    | BetweenRoundFrameOne Guess
    | Summary Votes

type alias Votes =
    { iVoted : Bool
    , timeout : Int
    , voted : Dict String ()
    }


type alias Game_ =
    { room : Room
    , canvas : Canvas
    , gamePart : GamePart
    , chat : Chat
    }


type Game = Game Game_


sync : Api.GameState -> Api.Name -> Game
sync { screen, history, scores } username =
    let (gamePart, makeRoom, canvasInit) = case screen of
        Api.Summary timeout votes ->
            ( Summary <| Votes True timeout votes
            , Room.joinBreak
            , Canvas.Demo
            )
        Api.RoundScores ->
            ( BetweenRound <| Guess.new Nothing 0
            , Room.joinBreak
            , Canvas.Demo
            )
        Api.Round { drawing, artist, timeout, word } ->
            let
                canvas = case word of
                    Just (Api.ForArtist _) -> Canvas.Sender drawing
                    _ -> Canvas.Receiver drawing
            in
                ( Round <| Guess.new word timeout
                , Room.joinRound artist
                , canvas
                )
        Api.Lobby master ->
            ( LobbyState { hideId = True }
            , Room.newInLobby master
            , Canvas.Demo
            )
    in
        Game
            { room = makeRoom username scores
            , canvas = Canvas.new canvasInit
            , gamePart = gamePart
            , chat = Chat.new username history
            }


type alias Event = Result Api.HiddenEvent Api.VisibleEvent

type Msg
    = Event Event
    | CanvasMsg Canvas.Msg
    | ChatMsg Chat.Msg
    | StartGame
    | TogglePassView
    | TickDown
    | SelectRoomid
    | UpdateTally
    | Vote

receive : Api.GameMsg -> Msg
receive msg =
    case msg of
        Api.CanvasMsg canvasMsg -> CanvasMsg <| Canvas.Server canvasMsg
        Api.VisibleEvent event -> Event <| Ok event
        Api.HiddenEvent event -> Event <| Err event

type GameCmd
    = Send Api.GameReq
    | Execute (Cmd Msg)
    | SendExecute Api.GameReq (Cmd Msg)

combineCmds : GameCmd -> GameCmd -> GameCmd
combineCmds gameCmd1 gameCmd2 =
    case (gameCmd1, gameCmd2) of
        (Send req1, Send req2) -> Send req1
        (Send req1, Execute cmd2) -> SendExecute req1 cmd2
        (Send req1, SendExecute req2 cmd2) -> SendExecute req1 cmd2
        (Execute cmd1, Send req2) -> SendExecute req2 cmd1
        (Execute cmd1, Execute cmd2) -> Execute (Cmd.batch [cmd1, cmd2])
        (Execute cmd1, SendExecute req2 cmd2) -> SendExecute req2 (Cmd.batch [cmd1, cmd2])
        (SendExecute req1 cmd1, Send req2) -> SendExecute req1 cmd1
        (SendExecute req1 cmd1, Execute cmd2) -> SendExecute req1 (Cmd.batch [cmd1, cmd2])
        (SendExecute req1 cmd1, SendExecute req2 cmd2) -> SendExecute req1 (Cmd.batch [cmd1, cmd2])

doNothing : GameCmd
doNothing = Execute (Cmd.none)


tickTimer : Game_ -> Game
tickTimer ({ gamePart } as game) =
    case gamePart of
        Round guess ->
            Game { game | gamePart = Round (Guess.update Guess.TickDown guess) }
        Summary v ->
            Game { game | gamePart = Summary { v | timeout = v.timeout - 1 } }
        _ -> Game game


togglePass : Game_ -> Game
togglePass ({ gamePart } as game) =
    case gamePart of
        LobbyState { hideId } ->
            Game { game | gamePart = LobbyState { hideId = not hideId } }
        _ -> Game game

voteLeave : Api.Name -> GamePart -> GamePart
voteLeave player state =
    let without = Dict.remove <| Api.showName player
    in  case state of
            Summary votes -> Summary { votes | voted = without votes.voted }
            anyelse -> anyelse

vote : GamePart -> GamePart
vote state =
    case state of
        Summary votes -> Summary { votes | iVoted = True }
        anyelse -> anyelse

update : Msg -> Game -> ( Game, GameCmd )
update msg (Game ({ canvas, chat } as game)) =
    case msg of
        Event event ->
            updateByEvent event game

        CanvasMsg canvasMsg ->
            let (newCanvas, canvasCmd) = Canvas.update canvasMsg canvas
                cmd = canvasCmd
                        |> Maybe.map (Send << Api.CanvasReq)
                        |> Maybe.withDefault doNothing
            in
                ( Game { game | canvas = newCanvas } , cmd )

        ChatMsg msg_ ->
            chatMsg msg_ doNothing chat
                |> mapFirst (\c -> Game { game | chat = c })

        TogglePassView -> (togglePass game, doNothing)
        StartGame -> (Game game, Send Api.ReqStart)
        TickDown -> (tickTimer game, doNothing)
        SelectRoomid -> (Game game, Execute <| Ports.selectRoomid ())
        Vote ->
            ( Game { game | gamePart = vote game.gamePart }
            , Send Api.ReqStart
            )
        UpdateTally ->
            case game.gamePart of
                BetweenRoundFrameOne guess ->
                    (Game { game | gamePart = BetweenRound guess }, doNothing)
                anyelse ->
                    Debug.log "update tally not shown" (Game game, doNothing)


chatMsg : Chat.Msg -> GameCmd -> Chat -> ( Chat, GameCmd )
chatMsg msg combine chat =
    let
        handleChatCmd chatCmd =
            case chatCmd of
                Chat.DoNothing -> doNothing
                Chat.Send text -> Send <| Api.ChatReq text
                Chat.UpdateScroll -> Execute <| Ports.bottomScrollChat ()
    in
        Chat.update msg chat
            |> mapSecond (combineCmds combine << handleChatCmd)


chatEvent : Api.VisibleEvent -> Chat -> ( Chat, GameCmd )
chatEvent event = chatMsg (Chat.receive event) doNothing

updateByEvent : Event -> Game_ -> ( Game, GameCmd )
updateByEvent event ({ room, canvas, gamePart, chat} as game) =
    let
        simpleChatMsg message =
            chatEvent message chat |> mapFirst (\c -> Game { game | chat = c })

        guessMsg message guess =
            let guess_ = Guess.update message guess
            in  (Game { game | gamePart = Round guess_ }, doNothing)

        startRound { timeout, artist, word } roomMod =
            let (newChat, chatCmd) = chatEvent (Api.EvStart artist) chat
                newGamePart = Round (Guess.new (Just word) timeout)
                newRoom = roomMod room
                newCanvas =
                    if Room.amArtist newRoom then
                        Canvas.new <| Canvas.Sender []
                    else
                        Canvas.new <| Canvas.Receiver []
                newGame = Game
                    { room = newRoom
                    , canvas = newCanvas
                    , gamePart = newGamePart
                    , chat = newChat
                    }
            in
                (newGame, chatCmd)

    in case (event, gamePart) of
        (Err (Api.EhSync gameState), _) ->
            (sync gameState (Room.myName room), doNothing)

        (Err Api.EhMastery, LobbyState _) ->
            (Game { game | room = Room.becomeMaster room } , doNothing)

        (Err (Api.EhReveal  i c), Round g) -> guessMsg (Guess.RevealOne i c) g
        (Err (Api.EhTimeout t  ), Round g) -> guessMsg (Guess.SetTimeout t) g
        (Err (Api.EhCorrect word), Round guess) ->
            let guess_ = Guess.update (Guess.RevealAll word) guess
                room_ = Room.guessed (Room.myName room) room
                (chat_, chatCmd) = chatMsg (Chat.correct word) doNothing chat
                part_ = Round guess_
            in
                ( Game { game | gamePart = part_, room = room_, chat = chat_ }
                , chatCmd
                )

        (Ok ((Api.EvMessage _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvStart   _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvOver    _) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvComplete ) as message), _) -> simpleChatMsg message
        (Ok ((Api.EvGuessed player) as msg), _) ->
            simpleChatMsg msg
                |> mapFirst
                    (\(Game g) -> Game { g | room = Room.guessed player room })

        (Ok ((Api.EvVoted voter) as msg), Summary votes) ->
            let name = Api.showName voter
                withVote = { votes | voted = Dict.insert name () votes.voted }
            in
                simpleChatMsg msg
                    |> mapFirst
                        (\(Game g) -> Game { g | gamePart = Summary withVote })

        (Ok ((Api.EvLeft player) as chatEvent_), _) ->
            let (newChat, chatCmd) = chatEvent chatEvent_ chat
                newRoom_ = Room.leaves player room
                newPart_ = voteLeave player gamePart
                myName = Room.myName room
                (newRoom, newCanvas, newPart) =
                    if Room.alone newRoom_ then
                        ( Room.newInLobby myName myName []
                        , Canvas.new Canvas.Demo
                        , LobbyState { hideId = True }
                        )
                    else
                        ( newRoom_, game.canvas, newPart_ )
            in
                ( Game
                    { chat = newChat
                    , room = newRoom
                    , gamePart = newPart
                    , canvas = newCanvas
                    }
                , chatCmd
                )

        (Ok ((Api.EvJoined player) as chatEvent_), Summary votes) ->
            let (chat_, chatCmd) = chatEvent chatEvent_ chat
                room_ = Room.joins player room
                n = Api.showName player
                voted = Summary { votes | voted = Dict.insert n () votes.voted }
            in
                ( Game { game | chat = chat_, room = room_, gamePart = voted }
                , chatCmd
                )

        (Ok ((Api.EvJoined player) as chatEvent_), _) ->
            let (newChat, chatCmd) = chatEvent chatEvent_ chat
                newRoom = Room.joins player room
            in
                (Game { game | chat = newChat , room = newRoom } , chatCmd)


        (Err (Api.EhOver word scores), Round guess ) ->
            let
                updateTally =
                    sleep (1 * second)
                        |> Task.perform (always UpdateTally)
                        |> Execute
                (newChat, chatCmd) =
                    chatMsg (Chat.receive <| Api.EvOver word) updateTally chat
                newRoom = Room.syncScores scores room
                newGuess = Guess.update (Guess.RevealAll word) guess
                newPart = BetweenRoundFrameOne newGuess
                newGame = Game
                    { game
                        | chat = newChat
                        , gamePart = newPart
                        , room = newRoom
                    }
            in
                (newGame, chatCmd)

        (Err (Api.EhStart arg), LobbyState _) ->
            startRound arg (Room.setArtist arg.artist << Room.newGame)

        (Err (Api.EhStart arg), Summary _) ->
            startRound arg (Room.setArtist arg.artist << Room.newGame)

        (Err (Api.EhStart arg), BetweenRoundFrameOne _) ->
            startRound arg (Room.setArtist arg.artist)

        (Err (Api.EhStart arg), BetweenRound _) ->
            startRound arg (Room.setArtist arg.artist)

        (Err (Api.EhComplete timeout scores), _) ->
            let (newChat, chatCmd) = chatEvent Api.EvComplete chat
                newRoom = Room.joinScores (Room.myName room) scores
                newCanvas = Canvas.new Canvas.Demo
                newGame = Game
                    { room = newRoom
                    , canvas = newCanvas
                    , gamePart = Summary <| Votes False timeout Dict.empty
                    , chat = newChat
                    }
            in
                (newGame, chatCmd)

        (anyElse, _) ->
            Debug.log "inconsistency" anyElse |> (always (Game game, doNothing))


subs : Game -> Sub Msg
subs (Game { gamePart }) =
    case gamePart of
        Round   _ -> every second (always TickDown)
        Summary _ -> every second (always TickDown)
        _ -> Sub.none


masterDialog : Bool -> Bool -> Api.RoomID -> Html Msg
masterDialog canStart hideId roomid =
    let
        roomdisplayAttributes =
            [ HA.type_ "text"
            , id "roomid"
            , class (if hideId then "hidden" else "display")
            , HA.value <| Api.showRoomLink roomid
            , HA.readonly True
            , HE.onClick SelectRoomid
            ]
        checkboxAttrs =
            [ HE.onClick TogglePassView
            , class "toggle"
            , HA.type_ "checkbox"
            ]
        startButton =
            if canStart then
                H.button [ HE.onClick StartGame ] [ text "Start the game!" ]
            else
                H.button [ HA.disabled True ] [ text "Needs 3 players" ]
    in
        div [ id "roomid-mod" ]
            [ div []
                [ b [] [ text "Room name:" ]
                , div [ id "roomid-border" ]
                    [ input roomdisplayAttributes []
                    , H.label [ class "toggle" ]
                        [ input checkboxAttrs [], H.span [] [] ]
                    ]
                ]
            , p [] [ text "share the room name with friends to let them join your game" ]
            , startButton
            ]


view : Api.RoomID -> Game -> Html Msg
view roomid (Game { room, gamePart, chat, canvas }) =
    let
        endView voteButton =
            div [ id "masterlayout" ]
                [ Room.view room
                , div [] [ Room.viewBoard room, voteButton ]
                , H.map ChatMsg <| Chat.view chat
                ]
        gameView topBar =
            div [ id "masterlayout" ]
                [ Room.view room
                , div [] [ topBar, H.map CanvasMsg <| Canvas.view canvas ]
                , H.map ChatMsg <| Chat.view chat
                ]
    in
        case gamePart of
            LobbyState { hideId }  ->
                let
                    flavorText = "The game leader is waiting to start the game"
                    topBar =
                        if Room.isMaster room then
                            masterDialog (Room.canStart room) hideId roomid
                        else
                            p [id "top-bar"] [ text flavorText]
                in
                    gameView topBar

            Round guess -> gameView (Guess.view guess)
            BetweenRoundFrameOne guess ->
                let tally = Room.viewPreviousTally room
                in  gameView (div [ id "tally-box" ] [Guess.view guess, tally])
            BetweenRound guess ->
                let tally = Room.viewRoundTally room
                in  gameView (div [ id "tally-box" ] [Guess.view guess, tally])

            Summary { iVoted, voted, timeout } ->
                let
                    necessary = Room.playerCount room // 2 + 1
                    counts =
                        toString (Dict.size voted) ++ "/" ++ toString necessary
                    timer =
                        div [ id "vote-timer" ] [ text <| toString timeout ]
                    buttonText =
                        [ text (if iVoted then "You voted!" else "Vote rematch")
                        , a [ id "vote-count" ] [ text counts ]
                        ]

                in
                    endView <| div [ id "vote-container" ]
                        [ button
                            [ onClick Vote, disabled iVoted, id "vote-button" ]
                            buttonText
                        , timer
                        ]

