module Tubes.Control where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

import Tubes.Model

eventToPlayerAction :: Event -> Maybe PlayerAction
eventToPlayerAction (EventKey (MouseButton LeftButton) Down _ point) = Just (PlayerStartAction point)
eventToPlayerAction (EventKey (MouseButton LeftButton) Up   _ point) = Just (PlayerCompleteAction point)
eventToPlayerAction (EventMotion point) = Just (PlayerSetPointer point)
eventToPlayerAction _ = Nothing

handleUniverse :: PlayerId -> Event -> Universe -> Universe
handleUniverse playerId event
  = case eventToPlayerAction event of
      Nothing -> id
      Just action -> applyPlayerAction action playerId

handleUniverseIO :: PlayerId -> Event -> TVar Universe -> IO (TVar Universe)
handleUniverseIO _ (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
handleUniverseIO playerId event w = do
  atomically $ modifyTVar w (handleUniverse playerId event)
  return w
