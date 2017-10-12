{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Tubes.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever, when)
import Control.Monad.Random (evalRand, newStdGen)
import Data.Binary (decode, encode)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Tubes

-- | Server config.
data Config = Config
  { configUniverse  :: TVar Universe              -- ^ The current state of the universe.
  , configClients   :: TVar (Map PlayerId Client) -- ^ All connected clients by a unique ID.
  , configPlayerIds :: TVar [PlayerId]            -- ^ Source of new player IDs.
  }

-- | A client is represented by its websocket 'Connection'.
type Client = Connection

-- | Default server config with empty universe and no clients.
newConfig :: IO Config
newConfig = do
  universe <- newRandomUniverse 3
  cfg <- atomically $ Config
          <$> newTVar universe
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
  spawnBot "Bot 1" newLineBot     (configUniverse cfg)
  spawnBot "Bot 2" extendLineBot  (configUniverse cfg)
  return cfg

-- | An API for the Game of Tubes server.
type TubesAPI = "connect" :> Raw

-- | The Game of Tubes server 'Application'.
server :: Config -> Server TubesAPI
server config = Tagged (websocketsOr defaultConnectionOptions wsApp backupApp)
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        playerId <- addClient conn config
        putStrLn $ playerId ++ " joined!"
        handleActions playerId conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Add a new client to the server state.
-- This will update 'configClients' and add
-- a new player to the 'configUniverse'.
addClient :: Client -> Config -> IO PlayerId
addClient client Config{..} =
  atomically $ do
    playerId:playerIds <- readTVar configPlayerIds
    writeTVar configPlayerIds playerIds
    modifyTVar configClients (Map.insert playerId client)
    modifyTVar configUniverse (addPlayer playerId)
    return playerId

-- | An infinite loop, receiving data from the 'Client'
-- and handling its actions via 'handlePlayerAction'.
handleActions :: PlayerId -> Connection -> Config -> IO ()
handleActions playerId conn cfg@Config{..} = forever $ do
  action <- decode <$> receiveData conn
  atomically $ do
    modifyTVar configUniverse (applyPlayerAction action playerId)

-- | Periodically update the 'Universe' and send updates to all the clients.
periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  clients <- readTVarIO configClients
  when (not (null clients)) $ do
    universe <- atomically $ do
      universe <- updateUniverse dt <$> readTVar configUniverse
      writeTVar configUniverse universe
      return universe
    broadcastUpdate universe cfg
  where
    -- FIXME: (ms / 10^6) is not the actual time that has passed since the previous update
    -- we should use getCurrentTime to more accurately keep track of time deltas
    dt = fromIntegral ms / 1000000

-- | Send every 'Client' updated 'Universe' concurrently.
broadcastUpdate :: Universe -> Config -> IO ()
broadcastUpdate universe cfg@Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (playerId, conn) = sendBinaryData conn (encode (universePlayers universe, universeTube universe))
      `catch` handleClosedConnection playerId

    handleClosedConnection :: PlayerId -> ConnectionException -> IO ()
    handleClosedConnection playerId _ = do
      putStrLn (playerId ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete playerId)
        modifyTVar configUniverse (kickPlayer playerId)
