{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (forever, mapM_)
import Data.Aeson
       (FromJSON(parseJSON), ToJSON, (.:), decode, defaultOptions, encode,
        genericToEncoding, toEncoding, withObject)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified Network.WebSockets as WS

type Client = WS.Connection

type ServerState = [Client]

broadcast :: ByteString -> ServerState -> IO ()
broadcast msg clients = do
    B8.putStrLn msg
    mapM_ (flip WS.sendTextData msg) clients

serverApp :: MVar ServerState -> WS.ServerApp
serverApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    clients <- readMVar state
    newclient <- return $ conn
    modifyMVar_ state $ return . (:) newclient
    talk conn state

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn state =
    forever $ do
        msg <- WS.receiveData conn
        readMVar state >>= broadcast msg

main :: IO ()
main = do
    state <- newMVar []
    WS.runServer "127.0.0.1" 9260 $ serverApp state
