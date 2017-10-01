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
trainWidth = 15

-- | Train length (in pixels).
trainLength :: Float
trainLength = 3 * trainWidth

-- | Track width (in pixels).
trackWidth :: Float
trackWidth = 0.75 * trainWidth

-- | Default track color.
defaultTrackColor :: Color
defaultTrackColor = greyN 0.5
