module Tubes.Config where

import Graphics.Gloss

-- | Train acceleration (pixels per second squared).
trainAcceleration :: Float
trainAcceleration = 1.5 * trainMaxSpeed

-- | Train maximum speed (pixels per second).
trainMaxSpeed :: Float
trainMaxSpeed = 5 * trainLength

-- | Train width (in pixels).
trainWidth :: Float
trainWidth = 40

-- | Train length (in pixels).
trainLength :: Float
trainLength = 2 * trainWidth

-- | Track width (in pixels).
trackWidth :: Float
trackWidth = 0.5 * trainWidth

-- | Single rail width.
railWidth :: Float
railWidth = 0.4 * trackWidth

endTrackLength :: Float
endTrackLength = 2 * stationRadius

-- | Radius of a regular station.
stationRadius :: Float
stationRadius = 0.8 * trainWidth

-- | Regular station color.
stationColor :: Color
stationColor = greyN 0.7

-- | Station maximum capacity (how many passengers can fit on a station).
stationCapacity :: Int
stationCapacity = 12

-- | Passenger render radius.
passengerRadius :: Float
passengerRadius = stationRadius / 5

-- | Passenger render color.
passengerColor :: Color
passengerColor = stationColor

-- | Number of seat rows on a train.
trainRows :: Int
trainRows = 4

-- | Number of seats in a single row in a train.
trainRowSeats :: Int
trainRowSeats = 2

-- | Total number of seats on a train.
trainCapacity :: Int
trainCapacity = trainRows * trainRowSeats

-- | An infinite list of different colors for different lines.
lineColors :: [Color]
lineColors = concat $ drop 1 $
  iterate (map dark) [red, green, blue, yellow, cyan, magenta, orange, white]

backgroundColor :: Color
backgroundColor = greyN 0.05

-- | How many passengers enter the system every second on average.
-- The rate depends on the size of the system (number of stations).
newPassengerRate :: Int -> Float
newPassengerRate n = sqrt (fromIntegral n)
