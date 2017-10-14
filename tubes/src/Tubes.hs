module Tubes (
  module Tubes.Bot,
  module Tubes.Config,
  module Tubes.Control,
  module Tubes.Model,
  module Tubes.Render,

  run,
  runWithBots,
) where

import Tubes.Bot
import Tubes.Config
import Tubes.Control
import Tubes.Model
import Tubes.Render

import Control.Concurrent.STM
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = do
  universe <- addPlayer "Nick" <$> newRandomUniverse 3
  play display bgColor fps universe renderUniverse (handleUniverse "Nick") updateUniverse
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = backgroundColor
    fps     = 60

    winSize = (800, 450)
    winOffset = (100, 100)

runWithBots :: IO ()
runWithBots = do
  universe <- addPlayer "Nick" <$> newRandomUniverse 3
  w <- newTVarIO universe
  spawnBot "The Extend Bot"   extendLineBot w
  spawnBot "The New Line Bot" newLineBot w
  playIO display bgColor fps w renderUniverseIO (handleUniverseIO "Nick") updateUniverseIO
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = backgroundColor
    fps     = 60

    winSize = (800, 450)
    winOffset = (100, 100)
