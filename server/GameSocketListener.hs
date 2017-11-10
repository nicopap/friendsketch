module GameSocketListener
    ( Cmd(Reply, Broadcast, ToAllOther)
    , Response(..)
    , Request(..)
    , ReactiveGame(..) -- To implement for games
    , Game
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
import API (Channel(..))
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
    connect
        :: Channel -> API.Name -> game
        -> Either SBS.ByteString (CommandM game)
    disconnect :: Channel -> API.Name -> game -> CommandM game
    receive :: API.Name -> Request -> game -> CommandM game
    init :: API.Name -> IO game


data Connections
    = Connections
        { canvas :: Maybe WS.Connection
        , info :: Maybe WS.Connection
        , chat :: Maybe WS.Connection
        }


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

data Response
    = CanvasResponse API.CanvasMsg
    | InfoResponse API.InfoMsg
    | ChatResponse API.ChatMsg
    deriving (Show)

data Request
    = CanvasRequest API.CanvasMsg
    | InfoRequest API.InfoRequest
    | ChatRequest API.ChatMsg
    deriving (Show)


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
    => Channel -> API.Name -> WS.PendingConnection
    -> MVar (Game game) -> IO ()
newGameConnection channel name pendingConn gameref = do
    waitLock <- newMVar ()
    modifyMVar_ gameref $ transformRoom waitLock
    takeMVar waitLock
    where
        transformRoom
            :: ReactiveGame game
            => MVar () -> Game game -> IO (Game game)
        transformRoom deathNotify Game{gameState,connections} = do
            maybeAccepted <- acceptIf $ connect channel name gameState
            case maybeAccepted of
                Just (conn, cmdM) -> do
                    () <- takeMVar deathNotify
                    _ <- Thread.forkFinally
                        (receiveLoop channel name conn gameref deathNotify)
                        (\_ -> putMVar deathNotify ())
                    newState <- execCmds name connections cmdM
                    return $! Game
                        { gameState = newState
                        , connections = addConn conn connections
                        }

                Nothing ->
                    return $! Game{gameState,connections}

        addConn conn =
            Map.alter (setConn channel conn) name

        setConn
            :: Channel -> WS.Connection
            -> Maybe Connections
            -> Maybe Connections
        setConn chan conn (Just old) =
            Just $ case chan of
                Info -> old {info = Just conn}
                Chat -> old {chat = Just conn}
                Canvas -> old {canvas = Just conn}
        setConn Info conn Nothing =
            Just Connections{info=Just conn,chat=Nothing,canvas=Nothing}
        setConn Canvas conn Nothing =
            Just Connections{info=Nothing,chat=Nothing,canvas=Just conn}
        setConn Chat conn Nothing =
            Just Connections{info=Nothing,chat=Just conn,canvas=Nothing}


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
    => Channel -> API.Name -> WS.Connection -> MVar (Game game)
    -> MVar () -> IO ()
receiveLoop channel name conn gameref notifyParent =
    Monad.finally loop finish
    where
        loop :: IO ()
        loop = Monad.forever $ do
            request <- decodeRequest channel <$> WS.receiveData conn
            case request of
                Nothing ->
                    WS.sendClose conn
                        ("Invalid format, please leave" :: SBS.ByteString)

                Just request' ->
                    log name (Left (channel, request'))
                    >> modifyMVar_ gameref (handleRequest request')

        finish :: IO ()
        finish = modifyMVar_ gameref closeConn >> putMVar notifyParent ()

        decodeRequest :: Channel -> LBS.ByteString -> Maybe Request
        decodeRequest Chat rawData = ChatRequest <$> Aeson.decode rawData
        decodeRequest Info rawData = InfoRequest <$> Aeson.decode rawData
        decodeRequest Canvas rawData = CanvasRequest <$> Aeson.decode rawData

        handleRequest :: ReactiveGame g => Request -> Game g -> IO (Game g)
        handleRequest request Game{gameState = oldState, connections} =
            receive name request oldState
                |>> execCmds name connections
                >=> (\newState -> Game{ gameState = newState, connections})

        closeConn :: ReactiveGame game => Game game -> IO (Game game)
        closeConn Game{gameState = oldState, connections} =
            disconnect channel name oldState
                |>> execCmds name newConns
                >=> ( \s -> Game{gameState=s, connections=newConns} )
            where
                newConns = Map.update (rmConn channel) name connections

                rmConn Info conns = Nothing
                rmConn Chat conns = Just conns{chat=Nothing}
                rmConn Canvas conns = Just conns{canvas=Nothing}


execCmds :: API.Name -> Map API.Name Connections -> CommandM a -> IO a
execCmds name connections CommandM{uncommand = (cmds, newState)} =
    Monad.forM_ cmds (cmdAction name connections)
    >> log name (Right cmds)
    >> (return $! newState)



sendChan :: Response -> Connections -> IO ()
sendChan response conns =
    case (response, conns) of
        (CanvasResponse msg, Connections{canvas}) ->
            maybeSend canvas $ Aeson.encode msg

        (InfoResponse msg, Connections{info}) ->
            maybeSend info $ Aeson.encode msg

        (ChatResponse msg, Connections{chat}) ->
            maybeSend chat $ Aeson.encode msg
    where
        maybeSend Nothing _ = return ()
        maybeSend (Just chan) textmsg =
            WS.sendTextData chan textmsg


cmdAction :: API.Name -> Map API.Name Connections -> Cmd -> IO ()
cmdAction name connections (Reply response) =
    Map.lookup name connections
        |> maybe (return ()) (sendChan response)
cmdAction _ connections (Broadcast response) =
    Map.elems connections
        |> Monad.mapM_ (sendChan response)
cmdAction name connections (ToAllOther response) =
    Map.assocs connections
        |> filter ((/= name) . fst)
        |> Monad.mapM_ (sendChan response . snd)


log :: API.Name -> Either (Channel, Request) [Cmd] -> IO ()
log name (Left (channel, request)) = do
    { putStr "> "; color Cyan; putStr $ show channel; reset
    ; color Blue; putStr $ " " ++ show name ++ "\n  "; reset
    ; putStr $ show request ++ "\n"
    } where
        color x = ANSI.setSGR [SetColor Foreground Dull x]
        reset = ANSI.setSGR []
log name (Right cmds) = do
    { putStr "< "; color Blue;
    ; putStr $ show name ++ "\n"
    ; reset
    ; Monad.mapM_ showCmd cmds
    } where
        color x = ANSI.setSGR [SetColor Foreground Dull x]
        reset = ANSI.setSGR []
        showCmd (Broadcast x) = do
            { putStr "  "
            ; color White; putStr "broadcast: "; reset
            ; showResponse x ; putStr "\n"
            }
        showCmd (ToAllOther x) = do
            { putStr "  "
            ; color White; putStr "toAllOther: "; reset
            ; showResponse x ; putStr "\n"
            }
        showCmd (Reply x) = do
            { putStr "  "
            ; color White; putStr "reply: "; reset
            ; showResponse x ; putStr "\n"
            }
        toTerm :: Aeson.ToJSON a => a -> IO ()
        toTerm = putStr . Text.unpack . Aeson.encodeToLazyText
        showResponse (CanvasResponse x) = toTerm x
        showResponse (ChatResponse x) = toTerm x
        showResponse (InfoResponse x) = toTerm x

