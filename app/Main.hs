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

    initialWorld = initTubeLine [ Segment (150, -200) (-150, -200)
                                , Segment (-150, -200) (-150, 200) ]

    renderWorld = renderTubeLine

    handleWorld (EventKey (MouseButton LeftButton) Down modifiers point) =
      case shift modifiers of
        Up    -> appendTubeLineSegment  point
        Down  -> prependTubeLineSegment point
    handleWorld _ = id

    -- move a single train along a straight track forwards and backwards
    updateWorld = updateTubeLineTrains

    winSize = (800, 450)
    winOffset = (100, 100)

