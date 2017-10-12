module Tubes.Control where

import Tubes.Model

import Graphics.Gloss.Interface.Pure.Game

handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (MouseButton LeftButton) Down _ point) = startPlayerAction point
handleUniverse (EventKey (MouseButton LeftButton) Up _ point) = completePlayerAction point
handleUniverse (EventMotion point) = setPlayerPointer point
handleUniverse _ = id
