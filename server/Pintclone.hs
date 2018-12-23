module Pintclone (Room, roomUsers) where

{-|
-}

import Prelude hiding (init)

import qualified System.Random as Random
import Data.ByteString as SBS (ByteString)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Function ((&))
import qualified Data.Set as Set

import qualified API
import GameSocketListener
    ( Cmd(Broadcast, Reply, ToAllOther), CommandM, addCmds
    , Request(..), Response(..)
    , Game(gameState), ReactiveGame(..)
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


data Pintclone
    = Pintclone
        { users :: Map API.Name [API.RoundSummary]
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

roomUsers :: Room -> [API.Name]
roomUsers room =
    room
     & gameState
     & users
     & Map.keys

(!) :: Pintclone -> [Cmd] -> CommandM Pintclone
(!) = flip addCmds

--  TODO: add proper error handling when duplicate websocket connections
connect' :: API.Name -> Pintclone -> Either SBS.ByteString (CommandM Pintclone)
connect' name state@Pintclone{section=Empty{expecting}}
  | name == expecting =
        let newState =
                state{users=Map.singleton name [],section=Lobby{master=name}}
        in Right $ newState ! [Reply $ toSync name newState]
  | otherwise = Left "You are not the room creator, how did that happen!?!"
connect' name state@Pintclone{users}
  | Map.notMember name users =
    Right $ state { users = Map.insert name [] users }
        ! [ ToAllOther $ API.MsgInfo $ API.Joined name ]
  | otherwise = Left "Name already taken"

disconnect' :: API.Name -> Pintclone -> CommandM Pintclone
disconnect' name state@Pintclone{users}
  | Map.member name users =
    state { users = Map.delete name users }
        ! [ Broadcast $ API.MsgInfo $ API.Left_ name ]

receive' :: API.Name -> Request -> Pintclone -> CommandM Pintclone
receive' name (API.ReqInfo API.ReqSync) state =
    state ! [Reply $ toSync name state]

receive' name (API.ReqInfo API.ReqStart) state@Pintclone{section=Lobby{master}}
  | name == master =
    newState ! [ Broadcast $ toSync name newState ]
    where
        newState = state { section = Playing 1 "Antelope" master Set.empty }

receive' name (API.ReqCanvas msg) state@Pintclone{section=Playing{artist}}
  | name == artist =
    state ! [ToAllOther $ API.MsgCanvas msg]

receive' name (API.ReqChat msg) state@Pintclone{section=Playing{}} =
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
    API.MsgInfo $ API.Sync $ case section of
        Empty{} ->
            undefined

        Lobby{master} ->
            API.Lobby $ API.LobbyState
                { players = Map.keys users
                , master = name == master
                }

        Playing{wordToGuess,artist} ->
            API.Round $ API.RoundState
                { players = Map.assocs users
                , artist = artist
                , timeout = 10
                }

        Scores ->
            API.Summary $ API.ScoresState {scores = Map.assocs users}

