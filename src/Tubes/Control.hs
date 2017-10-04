module Tubes.Control where

import Tubes.Model

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
