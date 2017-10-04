module Tubes.Render where

import Data.Function ((&))
import Data.Monoid
import Graphics.Gloss

import Tubes.Config
import Tubes.Model

-- | Render tube line with tracks and trains.
renderTubeLine :: TubeLine -> Picture
renderTubeLine tubeLine
  =   foldMap renderSegment (tubeLineSegments tubeLine)
  <>  foldMap renderStation (tubeLineStations tubeLine)
  <>  foldMap renderTrain   (tubeLineTrains tubeLine)

-- | Render a regular station.
renderStation :: Station -> Picture
renderStation (x, y) = (outerCircle <> innerCircle)
  & translate x y
  where
    r  = stationRadius
    r' = 0.6 * r

    outerCircle = thickCircle (r /2) r  & color stationColor
    innerCircle = thickCircle (r'/2) r' & color black

-- | Render a train.
renderTrain :: Train -> Picture
renderTrain train = renderLocomotive
  & rotate (- theta * 180 / pi)
  & translate x y
  & color red
  where
    (x, y) = trainPosition train
    theta  = trainOrientation train

-- | Render train's locomotive.
renderLocomotive :: Picture
renderLocomotive = polygon vertices
  & scale 0.5 0.5
  where
    vertices = [ (-w, -h), (-w, h), (w, h), (w + h, 0), (w, -h) ]
    h = trainWidth
    w = trainLength

-- | Render the segment train is currently running along.
renderTrainSegment :: Train -> Picture
renderTrainSegment = renderSegment . trainSegment

-- | Render tracks for a segment.
renderSegment :: Segment -> Picture
renderSegment s = (polygon leftRail <> polygon rightRail)
  & rotate (- theta * 180 / pi)
  & translate x y
  & color defaultTrackColor
  where
    (x, y) = segmentStart s
    theta  = segmentOrientation s
    leftRail  = [ (0,  r), (0,  h), (w,  h), (w,  r) ]
    rightRail = [ (0, -r), (0, -h), (w, -h), (w, -r) ]
    w = segmentLength s
    h = trackWidth / 2
    r = h - railWidth
