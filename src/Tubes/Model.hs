module Tubes.Model where

import Data.Maybe (listToMaybe)
import Data.List (foldl')

import Tubes.Config

-- | Point in 2D space.
type Point = (Float, Float)

-- | Station ID.
type StationId = Int

-- | TubeLine ID.
type TubeLineId = Int

-- | A station.
type Station = Point

-- | Tube system consisting of multiple lines and stations.
data Tube = Tube
  { tubeLines     :: [TubeLine]   -- ^ Lines.
  , tubeStations  :: [Station]    -- ^ Stations.
  }

-- | Initialise an empty tube.
initTube :: Tube
initTube = Tube [] []

-- | Add a station to the system.
addStation :: Station -> Tube -> Tube
addStation s tube = tube { tubeStations = s : tubeStations tube }

-- | Add a new tube line connecting two stations.
addTubeLine :: Station -> Station -> Tube -> Tube
addTubeLine s e tube = tube { tubeLines = initTubeLine [Segment s e] : tubeLines tube }

-- | Get a list of IDs for 'Tube' lines which go through a given 'Station'.
stationLines :: Station -> Tube -> [TubeLineId]
stationLines station = map fst . filter f . zip [0..] . tubeLines
  where
    f (_, line) = any (nearStation station) (tubeLineStations line)

-- | Update everything in a tube system with a given time interval.
updateTube :: Float -> Tube -> Tube
updateTube dt tube = tube
  { tubeLines = map (updateTubeLineTrains dt) (tubeLines tube) }

-- | 'Station' type relative to some 'TubeLine'.
data TubeLineStationType
  = TubeLineStartStation          -- ^ 'Station' is at the beginning of a 'TubeLine'.
  | TubeLineEndStation            -- ^ 'Station' is at the end of a 'TubeLine'.
  | TubeLineIntermediateStation   -- ^ 'Station' is somewhere between the ends of a 'TubeLine' (so it can't be continued from here).

-- | Find an element satisfying predicate and
-- return its indices from both ends of a list.
--
-- prop> fmap (\(i, j) -> i + j) (doubleIndexOf p xs) == length xs - 1
--
-- >>> doubleIndexOf (== 3) [1..10]
-- Just (2, 7)
-- >>> doubleIndexOf even [1, 3 .. 10]
-- Nothing
doubleIndexOf :: (a -> Bool) -> [a] -> Maybe (Int, Int)
doubleIndexOf p = eitherToMaybe . foldl' f (Left (0, 0))
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right x) = Just x

    f (Right (i, j)) _ = Right (i, j + 1)
    f (Left (i, j)) x
        | p x       = Right (i, j)
        | otherwise = Left (i + 1, j)

-- | Find out where on the line the station is.
tubeLineStationType :: Station -> TubeLine -> Maybe TubeLineStationType
tubeLineStationType station = fmap indicesToType . doubleIndexOf (nearStation station) . tubeLineStations
  where
    indicesToType (0, _)  = TubeLineStartStation
    indicesToType (_, 0)  = TubeLineEndStation
    indicesToType _       = TubeLineIntermediateStation

-- | If a 'Point' belongs to a 'Station' of a tube system then return it.
-- Otherwise return 'Nothing'.
pointToStation :: Point -> Tube -> Maybe Station
pointToStation point = listToMaybe . filter (nearStation point) . tubeStations

-- | Check if a 'Point' is near the 'Station'.
nearStation :: Point -> Station -> Bool
nearStation (x, y) (sx, sy) = sqrt ((sx - x)^2 + (sy - y)^2) < stationRadius

-- | A line with tracks and trains.
data TubeLine = TubeLine
  { tubeLineSegments  :: [Segment]    -- ^ Segments of which a line consists.
  , tubeLineTrains    :: [Train]      -- ^ Trains on the line.
  }

-- | Station index on a tube line.
type StationIndex = Int

-- | Create a new line with one new train.
initTubeLine :: [Segment] -> TubeLine
initTubeLine [] = TubeLine [] []
initTubeLine segments@(s:_) = TubeLine segments [initTrain s]

-- | Get a list of all stations on the line.
tubeLineStations :: TubeLine -> [Station]
tubeLineStations = segmentsToStations . tubeLineSegments
  where
    segmentsToStations [] = []
    segmentsToStations (Segment s e : ss) = s : e : map segmentEnd ss

-- | Modify a single 'TubeLine' in a tube system.
modifyTubeLine :: TubeLineId -> (TubeLine -> TubeLine) -> Tube -> Tube
modifyTubeLine tubeLineId f tube = tube
  { tubeLines = modifyAt tubeLineId f (tubeLines tube) }
  where
    modifyAt i f xs = case splitAt i xs of
      (ys, z:zs) -> ys ++ f z : zs
      _ -> xs

-- | Update all trains on the line.
-- Each train goes
updateTubeLineTrains :: Float -> TubeLine -> TubeLine
updateTubeLineTrains dtime tubeLine = tubeLine { tubeLineTrains = newTrains }
  where
    newTrains = map (updateTubeLineTrain dtime) (tubeLineTrains tubeLine)

    updateTubeLineTrain dt train
      = case moveTrain dt train of
          (newTrain, Nothing) -> newTrain
          (newTrain, Just leftoverTime) ->
            let (from, to, segment) = nextSegment train
            in updateTubeLineTrain leftoverTime (initTrain segment)
                  { trainFrom = from
                  , trainTo   = to
                  }

    nextSegment train
      -- forward motion
      | from < to && to < n = (to, to + 1, segments !! to)
      | from < to           = (to, from, mirrorSegment segment)
      -- backward motion
      | from > to && to > 0 = (to, to - 1, mirrorSegment (segments !! (to - 1)))
      | otherwise           = (to, from, segments !! to)
      where
        segment = trainSegment train
        from    = trainFrom train
        to      = trainTo train

        segments = tubeLineSegments tubeLine
        n = length segments

-- | A segment is a straight track the connects two points.
data Segment = Segment
  { segmentStart  :: Point    -- ^ Segment starting point.
  , segmentEnd    :: Point    -- ^ Segment end point.
  }

-- | Length of a segment.
segmentLength :: Segment -> Float
segmentLength s = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
  where
    (x1, y1) = segmentStart s
    (x2, y2) = segmentEnd s

-- | Switch start and end points of a segment.
mirrorSegment :: Segment -> Segment
mirrorSegment (Segment s e) = Segment e s

-- | A train.
data Train = Train
  { trainSegment      :: Segment      -- ^ Rail segment the train is on.
  , trainProgress     :: Float        -- ^ Time spent on this segment (in seconds).
  , trainLocation     :: Float        -- ^ Train location on the current segment (from start).
  , trainFrom         :: StationIndex -- ^ Station the train departed from recently.
  , trainTo           :: StationIndex -- ^ Station the train is headed to.
  }

-- | Compute train location on a linear track
-- of given length at a given time since start.
--
-- Note: negative time and overtime are ignored
-- and returned as a second result.
trainTrackLocation
  :: Float                  -- ^ Linear track length.
  -> Float                  -- ^ Time since start (in seconds).
  -> (Float, Maybe Float)   -- ^ Travelled path length and leftover time (if any).
trainTrackLocation d t
  | t < 0     = (0, Just t)
  | longTrack = trainLongTrackLocation
  | otherwise = trainShortTrackLocation
  where
    -- track length necessary to gain full speed
    accelTrackLength   = trainMaxSpeed^2 / trainAcceleration / 2

    -- long track means train gets full speed
    longTrack = d > 2 * accelTrackLength

    trainLongTrackLocation
      | accelerating  = (trainAcceleration * t^2 / 2, Nothing)
      | fullspeed     = (accelTrackLength + trainMaxSpeed * (t - fullspeedAccelTime), Nothing)
      | decelerating  = (decelLocation, Nothing)
      | otherwise     = (d, Just overtime)
      where
        accelerating = t < fullspeedAccelTime
        fullspeed    = t < fullspeedAccelTime + fullspeedTravelTime
        decelerating = overtime < 0
        overtime     = t - (2 * fullspeedAccelTime + fullspeedTravelTime)

        -- time since deceleration started
        t' = t - fullspeedAccelTime - fullspeedTravelTime

        -- time needed to gain full speed
        fullspeedAccelTime = trainMaxSpeed / trainAcceleration
        -- time spent with full speed
        fullspeedTravelTime = (d - 2 * accelTrackLength) / trainMaxSpeed

        decelLocation =
          accelTrackLength +                                  -- acceleration
          trainMaxSpeed * fullspeedTravelTime +               -- full speed
          trainMaxSpeed * t' - trainAcceleration * t'^2 / 2   -- deceleration

    trainShortTrackLocation
      | accelerating  = (trainAcceleration * t^2 / 2, Nothing)
      | decelerating  = (accelLength + decelLength, Nothing)
      | otherwise     = (d, Just overtime)
      where
        accelerating = t < accelTime
        decelerating = overtime < 0
        overtime     = t - 2 * accelTime

        -- time since deceleration started
        t' = t - accelTime

        accelLength = d / 2
        decelLength = maxSpeed * t' - trainAcceleration * t'^2 / 2
        -- time for acceleration/deceleration
        accelTime   = sqrt (2 * accelLength / trainAcceleration)
        -- max gained speed
        maxSpeed    = accelTime * trainAcceleration

-- | Move a train along its segment.
-- Any leftover time (negative or positive) is returned as a second result.
moveTrain :: Float -> Train -> (Train, Maybe Float)
moveTrain dt train = (newTrain, leftover)
  where
    newTrain = train
      { trainProgress = newProgress
      , trainLocation = newLocation
      }
    newProgress = trainProgress train + dt
    (newLocation, leftover) = trainTrackLocation (segmentLength (trainSegment train)) newProgress

-- | Initialise a train at the start of a segment.
initTrain :: Segment -> Train
initTrain s = Train
  { trainSegment  = s
  , trainProgress = 0
  , trainLocation = 0
  , trainFrom     = 0
  , trainTo       = 1
  }

-- | Compute actual train position.
trainPosition :: Train -> (Float, Float)
trainPosition train = (sx + p * (ex - sx), sy + p * (ey - sy))
  where
    (sx, sy) = segmentStart (trainSegment train)
    (ex, ey) = segmentEnd   (trainSegment train)
    p = trainLocation train / segmentLength (trainSegment train)

-- | Compute train orientation (angle in radians).
trainOrientation :: Train -> Float
trainOrientation = segmentOrientation . trainSegment

-- | Segment orientation (angle in radians).
segmentOrientation :: Segment -> Float
segmentOrientation s = atan2 (ey - sy) (ex - sx)
  where
    (sx, sy) = segmentStart s
    (ex, ey) = segmentEnd s

