module Tubes.Control where

import Data.Function ((&))
import Data.List (nub)

import Tubes.Model

-- | Add a new segment to the tube.
--
-- If @from@ station is the end of some line, it is extended.
--
-- If any station (or both) does not exist, they are created.
addSegment :: Point -> Point -> Tube -> Tube
addSegment from to tube
  | nearStation from to = tube
  | otherwise = newTube
      & addStation from
      & addStation to
  where
    newTube = case stationLines from tube of
      (i:_) -> case tubeLineStationType from (tubeLines tube !! i) of
        Just TubeLineStartStation -> modifyTubeLine i (prependTubeLineSegment to) tube
        Just TubeLineEndStation   -> modifyTubeLine i (appendTubeLineSegment to) tube
        _ -> addTubeLine from to tube
      _ -> addTubeLine from to tube

-- | Append a new segment to the end of the line.
appendTubeLineSegment :: Point -> TubeLine -> TubeLine
appendTubeLineSegment point tubeLine = tubeLine
  { tubeLineSegments = appendSegment (tubeLineSegments tubeLine) }
  where
    appendSegment []  = []
    appendSegment [segment] = [segment, Segment (segmentEnd segment) point]
    appendSegment (x:xs) = x : appendSegment xs

-- | Prepend a new segment to the beginning of the line.
prependTubeLineSegment :: Point -> TubeLine -> TubeLine
prependTubeLineSegment point tubeLine = tubeLine
  { tubeLineSegments = prependSegment (tubeLineSegments tubeLine)
  , tubeLineTrains   = map (updateStationIndices (+1)) (tubeLineTrains tubeLine)
  }
  where
    prependSegment []  = []
    prependSegment segments@(segment : _) = Segment point (segmentStart segment) : segments

    updateStationIndices f train = train
      { trainFrom = f (trainFrom train)
      , trainTo   = f (trainTo train)
      }
