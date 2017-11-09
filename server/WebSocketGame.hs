module WebSocketGame
    ( WebSocketGame(new, newConnection)
    , app
    ) where
{-| A game that accepts websocket client connections.
This exposes the functions necessary to the network-exposed side of the
application. GameSocketListener should simply greatly the complexity of
interacting with connections, it is a thin layer wrapper over the websocket
package interface to facilitate implementation of games over it.
-}

import Control.Concurrent (MVar)
import Data.Maybe (fromMaybe)

import qualified Network.WebSockets as WS
import Network.HTTP.Types.Status (badRequest400)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai as Wai

import qualified API


class WebSocketGame game where
    newConnection
        :: API.Channel -> API.Name -> WS.PendingConnection
        -> MVar game -> IO ()

    new :: API.Name -> IO (MVar game)


app
    :: WebSocketGame game
    => API.Channel -> API.Name -> MVar game
    -> Wai.Application
app channel name gameState request continuation =
    continuation $ fromMaybe
        (Wai.responseLBS badRequest400 [] "Not a WebSocket request")
        (maybeWaiRespond)
    where
        tryAddPending :: WS.ServerApp
        tryAddPending pending =
            newConnection channel name pending gameState

        maybeWaiRespond =
            WaiWS.websocketsApp
                WS.defaultConnectionOptions
                tryAddPending
                (request)
