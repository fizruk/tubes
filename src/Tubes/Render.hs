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
  <>  foldMap renderStation . tubeStations
  <>  foldMap (foldMap renderTrain   . tubeLineTrains)   . tubeLines

-- | Render a regular station.
renderStation :: Station -> Picture
renderStation station = (outerCircle <> innerCircle <> passengers)
  & translate x y
  where
    (x, y) = stationLocation station
    passengers = mconcat (zipWith renderStationPassenger (stationPassengers station) [0..])

    r  = stationRadius

    outerCircle = solidCircle r         & color stationColor
    innerCircle = solidCircle (0.6 * r) & color black

renderPassenger :: Passenger -> Picture
renderPassenger _ = solidCircle passengerRadius
  & color passengerColor

renderStationPassenger :: Passenger -> Int -> Picture
renderStationPassenger p n = renderPassenger p
  & translate 0 (stationRadius + 2 * passengerRadius)
  & rotate (360 * fromIntegral n / fromIntegral stationCapacity)

-- | Draw a solid circle of given radius.
solidCircle :: Float -> Picture
solidCircle r = thickCircle (r/2) r

-- | Render a train.
renderTrain :: Train -> Picture
renderTrain train = (renderLocomotive <> renderTrainPassengers (trainPassengers train))
  & rotate (- theta * 180 / pi)
  & translate x y
  & color red
  where
    (x, y) = trainPosition train
    theta = case trainDirection train of
      Forward  -> trainOrientation train
      Backward -> trainOrientation train + pi

renderTrainPassengers :: [Passenger] -> Picture
renderTrainPassengers = mconcat . zipWith renderTrainPassenger coords
  where
    renderTrainPassenger (i, j) p = renderPassenger p
      & translate (fromIntegral i * di - w/2) (fromIntegral j * dj - h/2)

    w = trainLength
    h = trainWidth

    di = w / fromIntegral (trainRows + 1)
    dj = h / fromIntegral (trainRowSeats + 1)

    coords =
      [ (i, j)
      | i <- [1..trainRows]
      , j <- [1..trainRowSeats]
      ]

-- | Render train's locomotive.
renderLocomotive :: Picture
renderLocomotive = (polygon vertices <> front <> back)
  & scale 0.5 0.5
  where
    vertices = [ (-w, -h), (-w, h), (w, h), (w, -h) ]
    front = solidCircle h & translate w 0
    back  = solidCircle h & translate (-w) 0
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
