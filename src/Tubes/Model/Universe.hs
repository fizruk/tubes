module Tubes.Model.Universe where

import Control.Monad.Random (runRand)
import Data.Maybe (fromMaybe)
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

data Universe = Universe
  { universeTube    :: Tube
  , universePlayer  :: Player
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
  , universePlayer  = initPlayer
  , universeGen     = g' }
  where
    (tube, g') = runRand (initRandomTube n) g

newRandomUniverse :: Int -> IO Universe
newRandomUniverse n = do
  g <- newStdGen
  return (initRandomUniverse n g)

updatePlayer :: (Player -> Player) -> Universe -> Universe
updatePlayer f u = u { universePlayer = f (universePlayer u) }

startPlayerAction :: Point -> Universe -> Universe
startPlayerAction point u = flip updatePlayer u $ \player ->
  player { playerAction = startAction point (universeTube u) }

setPlayerPointer :: Point -> Universe -> Universe
setPlayerPointer point u = flip updatePlayer u $ \player ->
  player { playerPointer = Just point }

completePlayerAction :: Point -> Universe -> Universe
completePlayerAction point u = u
  { universeTube = newTube
  , universePlayer = (universePlayer u) { playerAction = Nothing }
  }
  where
    tube = universeTube u
    newTube = fromMaybe tube $ do
      ia <- playerAction (universePlayer u)
      ca <- completeAction point ia tube
      return (handleAction ca tube)

