module Tubes.Config where

import Graphics.Gloss

-- | Train acceleration (pixels per second squared).
trainAcceleration :: Float
trainAcceleration = 400

-- | Train maximum speed (pixels per second).
trainMaxSpeed :: Float
trainMaxSpeed = 300

-- | Train width (in pixels).
trainWidth :: Float
trainWidth = 20

-- | Train length (in pixels).
trainLength :: Float
trainLength = 2.5 * trainWidth

-- | Track width (in pixels).
trackWidth :: Float
trackWidth = 0.5 * trainWidth

-- | Single rail width.
railWidth :: Float
railWidth = 0.33 * trackWidth

-- | Default track color.
defaultTrackColor :: Color
defaultTrackColor = greyN 0.7

-- | Radius of a regular station.
stationRadius :: Float
stationRadius = 0.8 * trainWidth

-- | Regular station color.
stationColor :: Color
stationColor = greyN 0.9

stationCapacity :: Int
stationCapacity = 12

passengerRadius :: Float
passengerRadius = stationRadius / 5

passengerColor :: Color
passengerColor = stationColor

trainRows :: Int
trainRows = 4

trainRowSeats :: Int
trainRowSeats = 2
