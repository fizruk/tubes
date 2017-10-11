{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Tubes.Model where

import Control.Monad.Random

import Data.Monoid
import Data.Function ((&))
import Data.List (minimumBy, partition, find)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)

import Tubes.Config

-- | Point in 2D space.
type Point = (Float, Float)

-- | Station ID.
type StationId = Int

-- | TubeLine ID.
type TubeLineId = Int

-- | A passenger is a user of a tube system.
data Passenger = Passenger
  { passengerDestination  :: Point  -- ^ Where is this passenger going.
  } deriving (Show)

-- | A station.
data Station = Station
  { stationLocation       :: Point        -- ^ Location of the station.
  , stationPassengers     :: [Passenger]  -- ^ Passengers waiting for a train on this station.
  } deriving (Show)

-- | Tube system consisting of multiple lines and stations.
data Tube = Tube
  { tubeLines     :: [TubeLine]   -- ^ Lines.
  , tubeStations  :: [Station]    -- ^ Stations.
  }

data TubeLineDirection
  = Forward
  | Backward
  deriving (Eq, Show)

data RouteDirection = RouteDirection
  { routeTubeLineId :: TubeLineId
  , routeDirection  :: TubeLineDirection
  , routeTo         :: Point
  } deriving (Show)

type Route = [RouteDirection]

-- | All possible routes from one point to another.
routes :: Point -> Point -> Tube -> [Route]
routes from' to tube = concatMap (routes' [] from') (stationLines from' tube)
  where
    routes' :: [TubeLineId] -> Point -> TubeLineId -> [Route]
    routes' visited from lineId
      | nearStation from to = pure []
      | otherwise = sameLineRoute ++ interchangeRoutes
        where
          line = tubeLines tube !! lineId

          sameLineRoute =
            [ [ RouteDirection lineId dir to]
            | dir <- directionsToStation from to line
            ]

          interchangeRoutes = concat
            [ map (RouteDirection lineId dir point :) nextRoutes
            | (newLineId, point) <- lineInterchanges lineId tube
            , newLineId `notElem` visited
            , dir <- directionsToStation from point line
            , let nextRoutes = routes' (lineId : visited) point newLineId
            ]

-- | Find a route with minimal interchanges (if exists).
minimalInterchangeRoute :: Point -> Point -> Tube -> Maybe Route
minimalInterchangeRoute from to tube
  = minimumBy (compareRouteLength) (fmap Just (routes from to tube) ++ [Nothing])
  where
    compareRouteLength :: Maybe Route -> Maybe Route -> Ordering
    compareRouteLength (Just xs) (Just ys) = comparing length xs ys
    compareRouteLength Nothing Nothing = EQ
    compareRouteLength Nothing _ = GT
    compareRouteLength _ Nothing = LT

-- | Which way to go by train on a given line to get from one station to another.
directionsToStation :: Point -> Point -> TubeLine -> [TubeLineDirection]
directionsToStation from to tubeLine
  = case dedup xs of
      (x:_:_:_)
        | nearStation x from -> [Forward, Backward]
        | otherwise          -> [Backward, Forward]
      [x, y]
        | nearStation x from && nearStation y to -> [Forward]
        | otherwise -> [Backward]
      _ -> []
  where
    dedup (x:y:ys)
      | x == y    = dedup (x:ys)
      | otherwise = x : dedup (y:ys)
    dedup ys = ys

    xs = filter (\p -> any (nearStation p) [from, to]) (tubeLineStations tubeLine)


handlePassenger :: Passenger -> Point -> Tube -> Maybe (TubeLineId, TubeLineDirection)
handlePassenger passenger point tube
  = case minimalInterchangeRoute point (passengerDestination passenger) tube of
      Just (RouteDirection lineId dir _ : _) -> Just (lineId, dir)
      _ -> Nothing

updatePassengersAt :: Station -> Train -> TubeLineId -> TubeLineDirection -> Tube -> (Station, Train)
updatePassengersAt station train lineId dir tube = (newStation, newTrain)
  where
    newStation = station { stationPassengers = stayingOnStation ++ unboarding ++ drop n boarding }
    newTrain = train { trainPassengers = stayingOnTrain ++ take n boarding }

    n = trainCapacity - length stayingOnTrain

    (unboarding, stayingOnTrain) = stayingLeavingPassengers (trainPassengers train)
    (stayingOnStation, boarding) = stayingLeavingPassengers (stationPassengers station)

    stayingLeavingPassengers passengers = both (map fst) $ partition snd
      [ (passenger, unboard)
      | passenger <- passengers
      , not (nearStation (passengerDestination passenger) (stationLocation station))
      , let action = handlePassenger passenger (stationLocation station) tube
      , let unboard = case action of
              Just (pLineId, pdir)
                | pLineId == lineId && pdir == dir -> False
                | otherwise -> True -- unboard when passenger needs to change line or direction here
              _ -> True
      ]

    both f (x, y) = (f x, f y)

-- | Initialise an empty tube.
initTube :: Tube
initTube = Tube [] []

-- | Initialise a station at a given location.
initStation :: Point -> Station
initStation point = Station
  { stationLocation   = point
  , stationPassengers = []
  }

spawnPassenger :: Point -> Point -> Tube -> Tube
spawnPassenger from to tube = modifyStationAt from addPassenger tube
  where
    addPassenger station = station { stationPassengers = Passenger to : stationPassengers station }

modifyStationAt :: Point -> (Station -> Station) -> Tube -> Tube
modifyStationAt point f tube = tube { tubeStations = map g (tubeStations tube) }
  where
    g station
      | nearStation point (stationLocation station) = f station
      | otherwise = station

modifyTrainAt :: TubeLineId -> Int -> (Train -> Train) -> Tube -> Tube
modifyTrainAt tubeLineId trainId f tube = tube
  & modifyTubeLine tubeLineId g
  where
    g line = line { tubeLineTrains = modifyAt trainId f (tubeLineTrains line) }

-- | Add a station to the system at a given location.
addStation :: Point -> Tube -> Tube
addStation s tube
  | isNewStation = tube { tubeStations = initStation s : tubeStations tube }
  | otherwise = tube
  where
    isNewStation = not (any (nearStation s) (map stationLocation (tubeStations tube)))

-- | Add a new tube line connecting two stations.
addTubeLine :: Point -> Point -> Tube -> Tube
addTubeLine = addTubeLineWith id

-- | Add a new tube line connecting two stations.
addTubeLineWith :: (TubeLine -> TubeLine) -> Point -> Point -> Tube -> Tube
addTubeLineWith f s e tube = tube { tubeLines = tubeLines tube ++ [f (initTubeLine [Segment s e])] }

-- | Get a list of IDs for 'Tube' lines which go through and stop at a given location.
stationLines :: Point -> Tube -> [TubeLineId]
stationLines station = map fst . filter f . zip [0..] . tubeLines
  where
    f (_, line) = any (nearStation station) (tubeLineStations line)

lineInterchanges :: TubeLineId -> Tube -> [(TubeLineId, Point)]
lineInterchanges tubeLineId tube =
  [ (newLineId, point)
  | point <- tubeLineStations (tubeLines tube !! tubeLineId)
  , newLineId <- stationLines point tube
  , newLineId /= tubeLineId
  ]

-- | Update everything in a tube system with a given time interval.
updateTube :: MonadRandom m => Float -> Tube -> m Tube
updateTube dt tube = tube { tubeLines = newLines }
  & handleEvents events
  & spawnRandomPassengers dt
  where
    (events, newLines) = sequenceA (zipWith (updateTubeLineTrains dt) [0..] (tubeLines tube))

spawnRandomPassengers :: MonadRandom m => Float -> Tube -> m Tube
spawnRandomPassengers dt tube = do
  k <- poisson (dt * newPassengerRate (length (tubeStations tube)))
  appEndo (mconcat (replicate k (Endo (>>= spawnRandomPassenger)))) (pure tube)

spawnRandomPassenger :: MonadRandom m => Tube -> m Tube
spawnRandomPassenger tube = do
  from <- randomStation tube
  to   <- randomStation tube
  return (spawnPassenger from to tube)

randomStation :: MonadRandom m => Tube -> m Point
randomStation tube = do
  i <- getRandomR (0, n - 1)
  return (stationLocation (stations !! i))
  where
    stations = tubeStations tube
    n = length stations

poisson :: MonadRandom m => Float -> m Int
poisson lambda = go 0 1
  where
    l = exp (- lambda)
    go k p
      | p > l = do
          u <- getRandomR (0, 1)
          go (k + 1) (p * u)
      | otherwise =
          pure (k - 1)

handleEvents :: [TrainStopEvent] -> Tube -> Tube
handleEvents events tube = foldr handleEvent tube events

handleEvent :: TrainStopEvent -> Tube -> Tube
handleEvent TrainStopEvent{..} tube = tube
  & modifyStationAt trainStopStation (const newStation)
  & modifyTrainAt trainStopTubeLine trainStopTrain (const newTrain)
  where
    Just station = getStationAt trainStopStation tube
    train = getTrainAt trainStopTubeLine trainStopTrain tube
    (newStation, newTrain) = updatePassengersAt station train trainStopTubeLine trainStopDirection tube

getStationAt :: Point -> Tube -> Maybe Station
getStationAt point = find (nearStation point . stationLocation) . tubeStations

getTrainAt :: TubeLineId -> Int -> Tube -> Train
getTrainAt lineId trainId = (!! trainId) . tubeLineTrains . (!! lineId) . tubeLines

addTrain :: TubeLine -> TubeLine
addTrain tubeLine = case tubeLineSegments tubeLine of
  (segment:_) -> tubeLine { tubeLineTrains = initTrain segment : tubeLineTrains tubeLine }
  _ -> tubeLine

-- | If a 'Point' belongs to a 'Station' of a tube system then return it.
-- Otherwise return 'Nothing'.
pointToStation :: Point -> Tube -> Maybe Point
pointToStation point = listToMaybe . filter (nearStation point) . map stationLocation . tubeStations

pointToTubeLineEnd :: Point -> Tube -> [(TubeLineId, TubeLineDirection)]
pointToTubeLineEnd point tube =
  [ (tubeLineId, dir)
  | (tubeLineId, tubeLine) <- zip [0..] (tubeLines tube)
  , let segments = tubeLineSegments tubeLine
  , (segment, dir) <- concat
      [ (,Backward) <$> take 1 segments
      , (,Forward)  <$> map flipSegment (take 1 (reverse segments))
      ]
  , nearSegmentStart point segment
  ]

flipSegment :: Segment -> Segment
flipSegment (Segment from to) = Segment to from

-- | Check if a 'Point' is near the 'Station'.
nearStation :: Point -> Point -> Bool
nearStation (x, y) (sx, sy) = sqrt ((sx - x)^2 + (sy - y)^2) < stationRadius

nearSegmentStart :: Point -> Segment -> Bool
nearSegmentStart p (Segment s e) = abs w < trackWidth/2 && l < 0 && l > (-endTrackLength)
  where
    n = normalizeV (e - s)
    w = dotV (p - s) (rotateV (pi/2) n)
    l = dotV (p - s) n

    dotV (x, y) (u, v) = x * u + y * v

    rotateV r (x, y) =
      ( x * cos r - y * sin r
      , x * sin r + y * cos r)

    normalizeV (x, y) = (d*x, d*y)
      where
        d = 1 / sqrt( x^2 + y^2 )

nearSegmentEnd :: Point -> Segment -> Bool
nearSegmentEnd p = nearSegmentStart p . flipSegment

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
tubeLineStations :: TubeLine -> [Point]
tubeLineStations = segmentsToStations . tubeLineSegments
  where
    segmentsToStations [] = []
    segmentsToStations (Segment s e : ss) = s : e : map segmentEnd ss

-- | Modify a single 'TubeLine' in a tube system.
modifyTubeLine :: TubeLineId -> (TubeLine -> TubeLine) -> Tube -> Tube
modifyTubeLine tubeLineId f tube = tube
  { tubeLines = modifyAt tubeLineId f (tubeLines tube) }

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = case splitAt i xs of
  (ys, z:zs) -> ys ++ f z : zs
  _ -> xs

data TrainStopEvent = TrainStopEvent
  { trainStopTrain      :: Int -- TrainIndex
  , trainStopStation    :: Point
  , trainStopTubeLine   :: TubeLineId
  , trainStopDirection  :: TubeLineDirection
  }

-- | Update all trains on the line.
-- Each train goes
updateTubeLineTrains :: Float -> TubeLineId -> TubeLine -> ([TrainStopEvent], TubeLine)
updateTubeLineTrains dtime tubeLineId tubeLine = (allEvents, tubeLine { tubeLineTrains = newTrains })
  where
    (allEvents, newTrains) = sequenceA (zipWith (updateTubeLineTrain [] dtime) [0..] (tubeLineTrains tubeLine))

    updateTubeLineTrain events dt i train
      = case moveTrain dt train of
          (newTrain, Nothing) -> (events, newTrain)
          (newTrain, Just leftoverTime) ->
            let (from, to, segment) = nextSegment train
                dir | from < to = Forward
                    | otherwise = Backward
                event = TrainStopEvent
                  { trainStopTrain      = i
                  , trainStopStation    = segmentStart segment
                  , trainStopTubeLine   = tubeLineId
                  , trainStopDirection  = dir
                  }
            in updateTubeLineTrain (event : events) leftoverTime i newTrain
                  { trainFrom     = from
                  , trainTo       = to
                  , trainSegment  = segment
                  , trainProgress = 0
                  , trainLocation = 0
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
  , trainPassengers   :: [Passenger]  -- ^ A list of passengers on board of a train.
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
  { trainSegment    = s
  , trainProgress   = 0
  , trainLocation   = 0
  , trainFrom       = 0
  , trainTo         = 1
  , trainPassengers = []
  }

-- | Compute actual train position.
trainPosition :: Train -> (Float, Float)
trainPosition train = (sx + p * (ex - sx), sy + p * (ey - sy))
  where
    (sx, sy) = segmentStart (trainSegment train)
    (ex, ey) = segmentEnd   (trainSegment train)
    p = trainLocation train / segmentLength (trainSegment train)

trainDirection :: Train -> TubeLineDirection
trainDirection train
  | trainFrom train < trainTo train = Forward
  | otherwise                       = Backward

-- | Compute train orientation (angle in radians).
trainOrientation :: Train -> Float
trainOrientation = segmentOrientation . trainSegment

-- | Segment orientation (angle in radians).
segmentOrientation :: Segment -> Float
segmentOrientation s = atan2 (ey - sy) (ex - sx)
  where
    (sx, sy) = segmentStart s
    (ex, ey) = segmentEnd s

