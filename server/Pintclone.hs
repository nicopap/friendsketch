module Pintclone (Room) where

{-|
-}

import Prelude hiding (init)

import qualified System.Random as Random
import Data.ByteString as SBS (ByteString)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified API
import GameSocketListener
    ( Cmd(Broadcast, Reply, ToAllOther), CommandM, addCmds
    , Request(..), Response(..)
    , Game, ReactiveGame(..)
    )

type Settings = ()

data GameSection
    = Empty { expecting :: API.Name }
    | Lobby { master :: API.Name }
    | Playing
        { round :: Int
        , wordToGuess :: Text
        , artist :: API.Name
        , correctlyGuessed :: Set API.Name
        }
    | Scores
    deriving (Show)


data Connections
    = CanvasChat
    | CanvasOnly
    | ChatOnly
    | Neither
    deriving (Show, Eq)


data Pintclone
    = Pintclone
        { users :: Map API.Name ([API.RoundSummary], Connections)
        , settings :: Settings
        , section :: GameSection
        , seed :: Random.StdGen
        , wordList :: [Text]
        }
    deriving (Show)


instance ReactiveGame Pintclone where
    connect = connect'
    disconnect = disconnect'
    receive = receive'
    init = init'

type Room = Game Pintclone

(!) :: Pintclone -> [Cmd] -> CommandM Pintclone
(!) = flip addCmds

--  TODO: add proper error handling when duplicate websocket connections
connect'
    :: API.Channel -> API.Name -> Pintclone
    -> Either SBS.ByteString (CommandM Pintclone)
connect' API.Info name state@Pintclone{section=Empty{expecting}}
  | name == expecting =
    let
        newState =
            state
                { users = Map.singleton name ([], Neither)
                , section = Lobby{master=name}
                }
    in
        Right $ newState ! [Reply $ toSync name newState]
  | otherwise =
    Left "You are not the room creator, how did that happen!?!"

connect' API.Info name state@Pintclone{users}
  | Map.notMember name users =
    Right $ state { users = Map.insert name ([], Neither) users }
        ! [ ToAllOther $ InfoResponse $ API.Joined name ]
  | otherwise =
    Left "Name already taken"

connect' API.Canvas name state@Pintclone{users} =
    Right $ return $! state { users = Map.adjust adjust name users }
    where
        adjust (x,Neither) = (x, CanvasOnly)
        adjust (x,ChatOnly) = (x, CanvasChat)
        adjust _ = undefined

connect' API.Chat name state@Pintclone{users} =
    Right $ return $! state { users = Map.adjust adjust name users }
    where
        adjust (x,Neither) = (x, ChatOnly)
        adjust (x,CanvasOnly) = (x, CanvasChat)
        adjust _ = undefined


disconnect' :: API.Channel -> API.Name -> Pintclone -> CommandM Pintclone
disconnect' API.Info name state@Pintclone{users}
  | Map.member name users =
    state { users = Map.delete name users }
        ! [ Broadcast $ InfoResponse $ API.Left_ name ]

disconnect' API.Canvas name state@Pintclone{users} =
    return $! state { users = Map.adjust adjust name users }
    where
        adjust (x,CanvasChat) = (x, ChatOnly)
        adjust (x,CanvasOnly) = (x, Neither)
        adjust _ = undefined

disconnect' API.Chat name state@Pintclone{users} =
    return $! state { users = Map.adjust adjust name users }
    where
        adjust (x,CanvasChat) = (x, CanvasOnly)
        adjust (x,ChatOnly) = (x, Neither)
        adjust _ = undefined


receive' :: API.Name -> Request -> Pintclone -> CommandM Pintclone
receive' name (InfoRequest API.ReqSync) state =
    state ! [Reply $ toSync name state]

receive' name (InfoRequest API.ReqStart) state@Pintclone{section=Lobby{master}}
  | name == master =
    newState ! [ Broadcast $ toSync name newState ]
    where
        newState = state { section = Playing 1 "Antelope" master Set.empty }

receive' name (CanvasRequest msg) state@Pintclone{section=Playing{artist}}
  | name == artist =
    state ! [ToAllOther $ CanvasResponse msg]

receive' name (ChatRequest msg) state@Pintclone{section=Playing{}} =
    undefined


init' :: API.Name -> IO Pintclone
init' name =
    (flip fmap) Random.getStdGen $ \seed -> Pintclone
        { users = Map.empty
        , settings = ()
        , section = Empty { expecting = name }
        , seed = seed
        , wordList = [ "bird", "cat", "ferret", "monkey", "city", "pool", "melon" ]
        }


toSync :: API.Name -> Pintclone -> Response
toSync name Pintclone{users,section} =
    InfoResponse $ API.Sync $ case section of
        Empty{} ->
            undefined

        Lobby{master} ->
            API.Lobby $ API.LobbyState
                { players = Map.keys users
                , master = name == master
                }

        Playing{wordToGuess,artist} ->
            API.Round $ API.RoundState
                { players = toAPIUsers users
                , artist = artist
                , timeout = 10
                }

        Scores ->
            API.Summary $ API.ScoresState {scores = toAPIUsers users}
    where
        toAPIUsers
            :: Map API.Name ([API.RoundSummary], Connections)
            -> [(API.Name, [API.RoundSummary])]
        toAPIUsers =
            map (\(a,(b,_)) -> (a,b)) . Map.assocs

