module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game

import Tubes

data GameState = GameState
  { gameTube   :: Tube
  , gameAction :: Maybe IncompleteAction
  }

startGameAction :: (Float, Float) -> GameState -> GameState
startGameAction point gs = gs { gameAction = startAction point (gameTube gs) }

completeGameAction :: (Float, Float) -> GameState -> GameState
completeGameAction point gs = gs
  { gameTube = newTube
  , gameAction = Nothing
  }
  where
    tube = gameTube gs
    newTube = fromMaybe tube $ do
      ia <- gameAction gs
      ca <- completeAction point ia tube
      return (handleAction ca tube)

newPassenger :: GameState -> GameState
newPassenger gs
  | null stations = gs
  | otherwise = gs { gameTube = spawnPassenger from to tube }
  where
    tube = gameTube gs
    stations = tubeStations tube
    from = stationLocation (head stations)
    to   = stationLocation (last stations)

main :: IO ()
main =
  play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = backgroundColor
    fps     = 60

    initialWorld = GameState
      { gameTube = initTube
      , gameAction = Nothing
      }

    renderWorld = renderTube . gameTube

    handleWorld (EventKey (MouseButton LeftButton) Down _ point) = startGameAction point
    handleWorld (EventKey (MouseButton LeftButton) Up _ point) = completeGameAction point
    handleWorld (EventKey (SpecialKey KeySpace) Down _ _) = newPassenger
    handleWorld _ = id

    -- move a single train along a straight track forwards and backwards
    updateWorld dt gs = gs { gameTube = updateTube dt (gameTube gs) }

    winSize = (800, 450)
    winOffset = (100, 100)

