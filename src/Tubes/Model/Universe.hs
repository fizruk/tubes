module Tubes.Model.Universe where

import Control.Monad.Random (runRand)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen, newStdGen)

import Tubes.Config
import Tubes.Model.Action
import Tubes.Model.Tube

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

data Player = Player
  { playerAction  :: Maybe IncompleteAction
  , playerPointer :: Maybe Point
  }

initPlayer :: Player
initPlayer = Player
  { playerAction  = Nothing
  , playerPointer = Nothing
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

updatePlayer :: PlayerId -> (Player -> Player) -> Universe -> Universe
updatePlayer playerId f u = u { universePlayers = Map.adjust f playerId (universePlayers u) }

startPlayerAction :: PlayerId -> Point -> Universe -> Universe
startPlayerAction playerId point u = flip (updatePlayer playerId) u $ \player ->
  player { playerAction = startAction point (universeTube u) }

setPlayerPointer :: PlayerId -> Point -> Universe -> Universe
setPlayerPointer playerId point u = flip (updatePlayer playerId) u $ \player ->
  player { playerPointer = Just point }

completePlayerAction :: PlayerId -> Point -> Universe -> Universe
completePlayerAction playerId point u = u
  { universeTube = newTube
  , universePlayers = Map.adjust (\player -> player { playerAction = Nothing}) playerId (universePlayers u)
  }
  where
    tube = universeTube u
    newTube = fromMaybe tube $ do
      player <- Map.lookup playerId (universePlayers u)
      ia <- playerAction player
      ca <- completeAction point ia tube
      return (handleAction ca tube)

