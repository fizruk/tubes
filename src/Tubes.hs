module Tubes (
  module Tubes.Config,
  module Tubes.Control,
  module Tubes.Model,
  module Tubes.Render,

  run,
) where

import Tubes.Config
import Tubes.Control
import Tubes.Model
import Tubes.Render

import Graphics.Gloss.Interface.Pure.Game

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

