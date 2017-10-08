module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game

import Tubes

data GameState = GameState
  { gameTube        :: Tube
  , gameStationFrom :: Maybe Station
  }

{-setStationFrom :: Point -> GameState -> GameState-}
setStationFrom point gs = gs
  { gameStationFrom = Just (fromMaybe point (pointToStation point (gameTube gs))) }

{-constructTube :: Point -> GameState -> GameState-}
constructTube point gs = gs
  { gameTube = case gameStationFrom gs of
      Just from -> addSegment from to (gameTube gs)
      _ -> gameTube gs
  , gameStationFrom = Nothing
  }
  where
    to = fromMaybe point (pointToStation point (gameTube gs))

main :: IO ()
main =
  play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = black
    fps     = 60

    initialWorld = GameState
      { gameTube = initTube
      , gameStationFrom = Nothing
      }

    renderWorld = renderTube . gameTube

    handleWorld (EventKey (MouseButton LeftButton) Down _ point) = setStationFrom point
    handleWorld (EventKey (MouseButton LeftButton) Up _ point) = constructTube point
    handleWorld _ = id

    -- move a single train along a straight track forwards and backwards
    updateWorld dt gs = gs { gameTube = updateTube dt (gameTube gs) }

    winSize = (800, 450)
    winOffset = (100, 100)

