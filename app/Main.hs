module Main where

import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game

import Tubes

main :: IO ()
main =
  play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = black
    fps     = 60

    initialWorld = initTrain (Segment (-300, -150) (200, 180))
    renderWorld = renderTrainSegment <> renderTrain
    handleWorld _ w = w

    -- move a single train along a straight track forwards and backwards
    updateWorld dt train
      = case moveTrain dt train of
          (newTrain, Nothing) -> newTrain
          (newTrain, Just leftover) -> updateWorld leftover (switchDirection newTrain)
      where
        switchDirection train = initTrain (switch (trainSegment train))
        switch (Segment s e) = Segment e s

    winSize = (800, 450)
    winOffset = (100, 100)

