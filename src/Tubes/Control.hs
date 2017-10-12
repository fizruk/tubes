module Tubes.Control where

import Tubes.Model

import Graphics.Gloss.Interface.Pure.Game

handleUniverse :: PlayerId -> Event -> Universe -> Universe
handleUniverse playerId (EventKey (MouseButton LeftButton) Down _ point) = startPlayerAction playerId point
handleUniverse playerId (EventKey (MouseButton LeftButton) Up _ point) = completePlayerAction playerId point
handleUniverse playerId (EventMotion point) = setPlayerPointer playerId point
handleUniverse _ _ = id
