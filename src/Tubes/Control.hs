module Tubes.Control where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

import Tubes.Model

handleUniverse :: PlayerId -> Event -> Universe -> Universe
handleUniverse playerId (EventKey (MouseButton LeftButton) Down _ point) = startPlayerAction playerId point
handleUniverse playerId (EventKey (MouseButton LeftButton) Up _ point) = completePlayerAction playerId point
handleUniverse playerId (EventMotion point) = setPlayerPointer playerId point
handleUniverse _ _ = id

handleUniverseIO :: PlayerId -> Event -> TVar Universe -> IO (TVar Universe)
handleUniverseIO _ (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
handleUniverseIO playerId event w = do
  atomically $ modifyTVar w (handleUniverse playerId event)
  return w
