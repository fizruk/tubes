{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Data.Binary (encode, decode)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets (Connection, receiveData, sendBinaryData, runClient)
import System.Exit (exitSuccess)

import Tubes

-- | Game state on client.
data ClientState = ClientState
  { clientUniverse    :: TVar Universe  -- ^ The current state of the universe.
  , clientConnection  :: Connection     -- ^ Websocket 'Connection' with the server.
  }

-- | Handle user input.
handleClient :: Event -> ClientState -> IO ClientState
handleClient (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
handleClient event cs@ClientState{..} =
  case eventToPlayerAction event of
    Just action -> do
      -- fork to avoid interface freezing
      _ <- forkIO $ sendBinaryData clientConnection (encode action)
      return cs
    _ -> return cs

-- | Handle 'Universe' updates coming from server.
handleUpdates :: ClientState -> IO ()
handleUpdates ClientState{..} = forever $ do
  (players, tube) <- decode <$> receiveData clientConnection
  atomically $ modifyTVar clientUniverse (\universe -> universe { universeTube = tube, universePlayers = players })

-- | Draw the current state of the 'Universe'.
renderClient :: ClientState -> IO Picture
renderClient ClientState{..} = renderUniverseIO clientUniverse

-- | This does nothing since updates come from server.
-- See 'handleUpdates'.
updateClient :: Float -> ClientState -> IO ClientState
updateClient _dt cs = return cs

main :: IO ()
main = do
  universe <- newTVarIO emptyUniverse
  runClient "127.0.0.1" 8000 "/connect" $ \conn -> do
    let cs = ClientState universe conn
    _ <- forkIO (handleUpdates cs)
    playIO display bgColor fps cs renderClient handleClient updateClient
  where
    display   = InWindow "Tubes" winSize winOffset
    bgColor   = backgroundColor
    fps       = 60

    winSize   = (800, 450)
    winOffset = (100, 100)
