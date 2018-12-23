module GameSocketListener
    ( Cmd(Reply, Broadcast, ToAllOther)
    , Response
    , Request
    , ReactiveGame(..) -- To implement for games
    , Game(gameState)
    , CommandM
    , addCmds
    ) where

{- The interface between the real world and a game. The idea is to
decouple the websocket logic from the game logic.

GameSocketListener should simplify greatly the complexity of
interacting with connections, it is a thin layer wrapper over the websocket
package interface to facilitate implementation of games over it.

To achieve this, we somehow need to encapsulate client requests and
game "reaction".

Thankfully, the game only "reacts" in a limited manner. Most notably,
the following happens:

* The game state changes,
* Something is broadcasted to all connected clients,
* Something is broadcasted to all connected clients but the
  requester,
* Something is sent the client that made the request,
* Something is sent to an arbitrary client.
* Some timeout is triggered to latter apply a modification to the
  game state.

We can define an *extremly* limited set of actions that the game
should be able to request.

----

There should be an exposed type facilitating the handling of
connections. Having a minimal API on the Request side would
be lovely.

To keep track of state within receive loops, I need MVars.
The MVar need to contain the updated list of connections
AND the game state.

-}

import Prelude hiding (init, log)
import qualified Control.Concurrent as Thread
import Control.Concurrent (MVar, newMVar, modifyMVar_, putMVar, takeMVar)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Monad as Monad
import qualified Control.Exception as Monad (finally)

import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString as SBS (ByteString)
import qualified Data.Text.Lazy as Text
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified System.Console.ANSI as ANSI
import System.Console.ANSI.Types
    ( Color(Cyan, Blue, White)
    , SGR(SetColor, Reset)
    , ConsoleLayer(Foreground)
    , ColorIntensity(Dull)
    )

import WebSocketGame (WebSocketGame(..))
import qualified API


(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(|>>) :: a -> (a -> b) -> b
(|>>) = flip ($)
infixl 1 |>>

(>=>) :: Functor m => m a -> (a -> b) -> m b
(>=>) = flip fmap
infixl 1 >=>

class ReactiveGame game where
    connect :: API.Name -> game -> Either SBS.ByteString (CommandM game)
    disconnect :: API.Name -> game -> CommandM game
    receive :: API.Name -> Request -> game -> CommandM game
    init :: API.Name -> IO game


data Connections = Connections { wschan :: WS.Connection }


data Game game
    = Game
        { gameState :: game
        , connections :: Map API.Name Connections
        }

instance Show (Game game) where
    show Game{connections} =
        "Game { <GAMESTATE>, <"
        ++ show (Map.keys connections)
        ++ "> }"


instance ReactiveGame game => WebSocketGame (Game game) where
    newConnection = newGameConnection
    new = newGame

newtype CommandM game
    = CommandM { uncommand :: ([Cmd], game) }
    deriving (Functor, Applicative, Monad)

data Cmd
    = Reply Response
    | Broadcast Response
    | ToAllOther Response
    deriving (Show)
    -- | ToName API.Name Response
    -- | Timeout Int (game -> CommandM game)
    -- | Random

type Response = API.GameMsg

type Request = API.GameReq


addCmds :: [Cmd] -> x -> CommandM x
addCmds cmds = CommandM . (,) cmds


newGame :: ReactiveGame game => API.Name -> IO (MVar (Game game))
newGame initialName =
    init initialName
    >>= \initGame -> newMVar $ Game
        { gameState = initGame
        , connections = Map.empty
        }


{-|
gather response from model
if response successful,
    accept request
    add new connection to connection list
    setup request to modify model in receive loop
    executes commands
otherwise
    reject request
-}
newGameConnection
    :: ReactiveGame game
    => API.Name -> WS.PendingConnection
    -> MVar (Game game) -> IO ()
newGameConnection name pendingConn gameref = do
    waitLock <- newMVar ()
    modifyMVar_ gameref $ transformRoom waitLock
    takeMVar waitLock
    where
        transformRoom :: ReactiveGame game
            => MVar () -> Game game -> IO (Game game)
        transformRoom deathNotify Game{gameState,connections} = do
            maybeAccepted <- acceptIf $ connect name gameState
            case maybeAccepted of
                Just (conn, cmdM) -> do
                    () <- takeMVar deathNotify
                    _ <- Thread.forkFinally
                        (receiveLoop name conn gameref deathNotify)
                        (\_ -> putMVar deathNotify ())
                    newState <- execCmds name connections cmdM
                    return $! Game
                        { gameState = newState
                        , connections = addConn conn connections
                        }

                Nothing ->
                    return $! Game{gameState,connections}

        addConn conn = Map.insert name (Connections{wschan = conn})

        acceptIf :: Either SBS.ByteString a -> IO (Maybe (WS.Connection, a))
        acceptIf response =
            case response of
                Left errormsg ->
                    WS.rejectRequest pendingConn errormsg
                    >> (return $! Nothing)

                Right val -> do
                    conn <- WS.acceptRequest pendingConn
                    WS.forkPingThread conn 30
                    return $! Just (conn, val)


{-|
Recieve until connection is closed. Manipulates Game
according to what the instance of ReactiveGame decided the reactions
of received data to be.
-}
receiveLoop
    :: ReactiveGame game
    => API.Name -> WS.Connection -> MVar (Game game)
    -> MVar () -> IO ()
receiveLoop name conn gameref notifyParent =
    Monad.finally loop finish
    where
        loop :: IO ()
        loop = Monad.forever $ do
            request <- Aeson.decode <$> WS.receiveData conn
            case request of
                Nothing ->
                    WS.sendClose conn
                        ("Invalid format, please leave" :: SBS.ByteString)

                Just request' ->
                    log name (Left request')
                    >> modifyMVar_ gameref (handleRequest request')

        finish :: IO ()
        finish = modifyMVar_ gameref closeConn >> putMVar notifyParent ()

        handleRequest :: ReactiveGame g => Request -> Game g -> IO (Game g)
        handleRequest request Game{gameState = oldState, connections} =
            receive name request oldState
                |>> execCmds name connections
                >=> (\newState -> Game{ gameState = newState, connections})

        closeConn :: ReactiveGame game => Game game -> IO (Game game)
        closeConn Game{gameState = oldState, connections} =
            disconnect name oldState
                |>> execCmds name newConns
                >=> ( \s -> Game{gameState=s, connections=newConns} )
            where
                newConns = Map.delete name connections


execCmds :: API.Name -> Map API.Name Connections -> CommandM a -> IO a
execCmds name connections CommandM{uncommand = (cmds, newState)} =
    Monad.forM_ cmds (cmdAction name connections)
    >> log name (Right cmds)
    >> (return $! newState)



send :: Response -> Connections -> IO ()
send response Connections{wschan} =
    WS.sendTextData wschan $ Aeson.encode response


cmdAction :: API.Name -> Map API.Name Connections -> Cmd -> IO ()
cmdAction name connections (Reply response) =
    Map.lookup name connections
        |> maybe (return ()) (send response)
cmdAction _ connections (Broadcast response) =
    Map.elems connections
        |> Monad.mapM_ (send response)
cmdAction name connections (ToAllOther response) =
    Map.assocs connections
        |> filter ((/= name) . fst)
        |> Monad.mapM_ (send response . snd)


log :: API.Name -> Either Request [Cmd] -> IO ()
log name (Left request) = do
    { putStr "> "; color Cyan; reset
    ; color Blue; putStr $ " " ++ show name ++ "\n  "; reset
    ; putStr $ show request ++ "\n"
    } where
        color x = ANSI.setSGR [SetColor Foreground Dull x]
        reset = ANSI.setSGR []
log name (Right cmds) = do
    { putStr "< "; color Blue
    ; putStr $ show name ++ "\n"
    ; reset
    ; Monad.mapM_ showCmd cmds
    } where
        color x = ANSI.setSGR [SetColor Foreground Dull x]
        reset = ANSI.setSGR []
        showCmd (Broadcast x) = do
            { putStr "  "
            ; color White; putStr "broadcast: "; reset
            ; toTerm x ; putStr "\n"
            }
        showCmd (ToAllOther x) = do
            { putStr "  "
            ; color White; putStr "toAllOther: "; reset
            ; toTerm x ; putStr "\n"
            }
        showCmd (Reply x) = do
            { putStr "  "
            ; color White; putStr "reply: "; reset
            ; toTerm x ; putStr "\n"
            }
        toTerm :: Aeson.ToJSON a => a -> IO ()
        toTerm = putStr . Text.unpack . Aeson.encodeToLazyText

