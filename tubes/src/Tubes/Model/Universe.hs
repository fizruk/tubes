{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Tubes.Model.Universe where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Control.Monad.Random (runRand)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen, newStdGen, mkStdGen)

import Data.Binary (Binary)
import GHC.Generics (Generic)

import Tubes.Config
import Tubes.Model.Action
import Tubes.Model.Tube

updateUniverseIO :: Float -> TVar Universe -> IO (TVar Universe)
updateUniverseIO dt w = do
  atomically $ modifyTVar w (updateUniverse dt)
  return w

updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = u
  { universeTube = newTube
  , universeGen  = newGen
  }
  where
    (newTube, newGen) = runRand (updateTube (gameSpeed * dt) (universeTube u)) (universeGen u)

type PlayerId = String

data Universe = Universe
  { universeTube    :: Tube
  , universePlayers :: Map PlayerId Player
  , universeGen     :: StdGen
  }

data PlayerAction
  = PlayerStartAction     Point
  | PlayerCompleteAction  Point
  | PlayerSetPointer      Point
  deriving (Generic, Binary)

data Player = Player
  { playerAction  :: Maybe IncompleteAction
  , playerPointer :: Maybe Point
  } deriving (Generic, Binary)

initPlayer :: Player
initPlayer = Player
  { playerAction  = Nothing
  , playerPointer = Nothing
  }

emptyUniverse :: Universe
emptyUniverse = Universe
  { universeTube    = initTube
  , universePlayers = Map.empty
  , universeGen     = mkStdGen 0
  }

initRandomUniverse :: Int -> StdGen -> Universe
initRandomUniverse n g = Universe
  { universeTube    = tube
  , universePlayers = Map.empty
  , universeGen     = g' }
  where
    (tube, g') = runRand (initRandomTube n) g

newRandomUniverse :: Int -> IO Universe
newRandomUniverse n = do
  g <- newStdGen
  return (initRandomUniverse n g)

addPlayer :: PlayerId -> Universe -> Universe
addPlayer playerId u = u { universePlayers = Map.insert playerId initPlayer (universePlayers u) }

kickPlayer :: PlayerId -> Universe -> Universe
kickPlayer playerId u = u { universePlayers = Map.delete playerId (universePlayers u) }

updatePlayer :: PlayerId -> (Player -> Player) -> Universe -> Universe
updatePlayer playerId f u = u { universePlayers = Map.adjust f playerId (universePlayers u) }

applyPlayerAction :: PlayerAction -> PlayerId -> Universe -> Universe
applyPlayerAction (PlayerStartAction     point) = playerStartAction    point
applyPlayerAction (PlayerCompleteAction  point) = playerCompleteAction point
applyPlayerAction (PlayerSetPointer      point) = playerSetPointer     point

playerStartAction :: Point -> PlayerId -> Universe -> Universe
playerStartAction point playerId u = flip (updatePlayer playerId) u $ \player ->
  player { playerAction = startAction point (universeTube u) }

playerSetPointer :: Point -> PlayerId -> Universe -> Universe
playerSetPointer point playerId u = flip (updatePlayer playerId) u $ \player ->
  player { playerPointer = Just point }

playerCompleteAction :: Point -> PlayerId -> Universe -> Universe
playerCompleteAction point playerId u = u
  { universeTube = newTube
  , universePlayers = Map.adjust (\player -> player { playerAction = Nothing}) playerId (universePlayers u)
  }
  where
    tube = universeTube u
    mca = do
          player <- Map.lookup playerId (universePlayers u)
          ia <- playerAction player
          completeAction point ia tube
    newTube = fromMaybe tube $ do
      ca <- mca
      return (applyCompleteAction ca tube)

applyPlayerCompleteAction :: PlayerId -> CompleteAction -> Universe -> Universe
applyPlayerCompleteAction _playerId action u = u { universeTube = applyCompleteAction action (universeTube u) }
