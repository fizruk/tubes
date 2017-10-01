module Tubes.Model where

import Tubes.Config

-- | Point in 2D space.
type Point = (Float, Float)

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

-- | A train.
data Train = Train
  { trainSegment      :: Segment      -- ^ Rail segment the train is on.
  , trainProgress     :: Float        -- ^ Time spent on this segment (in seconds).
  , trainLocation     :: Float        -- ^ Train location on the current segment (from start).
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

