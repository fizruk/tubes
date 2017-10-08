module Tubes.Render where

import Data.Function ((&))
import Data.Monoid
import Graphics.Gloss

import Tubes.Config
import Tubes.Model

-- | Render the whole tube system.
renderTube :: Tube -> Picture
renderTube
  =   foldMap (foldMap renderSegment . tubeLineSegments) . tubeLines
  <>  foldMap (foldMap renderStation . tubeLineStations) . tubeLines
  <>  foldMap (foldMap renderTrain   . tubeLineTrains)   . tubeLines

-- | Render tube line with tracks and trains.
renderTubeLine :: TubeLine -> Picture
renderTubeLine
  =   foldMap renderSegment . tubeLineSegments
  <>  foldMap renderStation . tubeLineStations
  <>  foldMap renderTrain   . tubeLineTrains

-- | Render a regular station.
renderStation :: Station -> Picture
renderStation (x, y) = (outerCircle <> innerCircle)
  & translate x y
  where
    r  = stationRadius

    outerCircle = solidCircle r         & color stationColor
    innerCircle = solidCircle (0.6 * r) & color black

-- | Draw a solid circle of given radius.
solidCircle :: Float -> Picture
solidCircle r = thickCircle (r/2) r

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
