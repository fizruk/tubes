module Tubes.Render where

import Data.Function ((&))
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Tubes.Config
import Tubes.Model

-- | Render the whole tube system.
renderTube :: Tube -> Picture
renderTube
  =   renderTubeLines . tubeLines
  <>  foldMap renderStation . tubeStations
  <>  renderTrains . map tubeLineTrains . tubeLines

renderTubeLines :: [TubeLine] -> Picture
renderTubeLines = mconcat . zipWith renderTubeLine lineColors

renderTubeLine :: Color -> TubeLine -> Picture
renderTubeLine lineColor
  =   foldMap (renderSegment lineColor) . tubeLineSegments
  <>  renderTubeLineEnds lineColor

renderTubeLineEnds :: Color -> TubeLine -> Picture
renderTubeLineEnds lineColor tubeLine =
  case tubeLineSegments tubeLine of
    []     -> blank
    [s]    -> renderSegmentStart s <> renderSegmentEnd s
    (s:ss) -> renderSegmentStart s <> renderSegmentEnd (last ss)
  where
    renderSegmentStart :: Segment -> Picture
    renderSegmentStart (Segment s e) = renderSegment lineColor (Segment s s')
      where
        s' = s + mulSV endTrackLength (normalizeV (s - e))

    renderSegmentEnd :: Segment -> Picture
    renderSegmentEnd (Segment s e) = renderSegmentStart (Segment e s)

renderTrains :: [[Train]] -> Picture
renderTrains = mconcat . zipWith (foldMap . renderTrain) lineColors

-- | Render a regular station.
renderStation :: Station -> Picture
renderStation station = (liningCircle <> outerCircle <> innerCircle <> passengers)
  & translate x y
  where
    (x, y) = stationLocation station
    passengers = mconcat (zipWith renderStationPassenger (stationPassengers station) [0..])

    r  = stationRadius

    liningCircle = solidCircle (1.0 * r) & color backgroundColor
    outerCircle  = solidCircle (0.8 * r) & color stationColor
    innerCircle  = solidCircle (0.5 * r) & color backgroundColor

renderPassenger :: Passenger -> Picture
renderPassenger _ = lining <> passenger
  where
    r = passengerRadius
    lining    = solidCircle (1.0 * r) & color backgroundColor
    passenger = solidCircle (0.8 * r) & color passengerColor

renderStationPassenger :: Passenger -> Int -> Picture
renderStationPassenger p n = renderPassenger p
  & translate 0 (stationRadius + (2 + k) * passengerRadius)
  & rotate (360 * (0.4 * k + fromIntegral n) / fromIntegral stationCapacity)
  where
    k = fromIntegral (n `div` stationCapacity)

-- | Draw a solid circle of given radius.
solidCircle :: Float -> Picture
solidCircle r = thickCircle (r/2) r

-- | Render a train.
renderTrain :: Color -> Train -> Picture
renderTrain trainColor train
  = (renderLocomotive trainColor <> renderTrainPassengers (trainPassengers train))
  & rotate (- theta * 180 / pi)
  & translate x y
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
renderLocomotive :: Color -> Picture
renderLocomotive trainColor
  =   color backgroundColor (loco trainLength trainWidth)
  <>  color trainColor (loco trainLength (trainWidth * 0.8))
  where
    loco w h = (polygon (vertices w h) <> front w h <> back w h)
      & scale 0.5 0.5

    vertices w h = [ (-w, -h), (-w, h), (w, h), (w, -h) ]
    front    w h = solidCircle h & translate w 0
    back     w h = solidCircle h & translate (-w) 0

-- | Render tracks for a segment.
renderSegment :: Color -> Segment -> Picture
renderSegment segmentColor s = foldMap polygon [ leftRail, rightRail , start, end ]
  & rotate (- theta * 180 / pi)
  & translate x y
  & color segmentColor
  where
    (x, y) = segmentStart s
    theta  = segmentOrientation s
    leftRail  = [ (0,  r), (0,  h), (w,  h), (w,  r) ]
    rightRail = [ (0, -r), (0, -h), (w, -h), (w, -r) ]
    start     = [ (0, sr), (railWidth, sr), (railWidth, -sr), (0, -sr) ]
    end       = [ (w, sr), (w-railWidth, sr), (w-railWidth, -sr), (w, -sr) ]
    w = segmentLength s
    h = trackWidth / 2
    r = h - railWidth

    sr = stationRadius - railWidth
