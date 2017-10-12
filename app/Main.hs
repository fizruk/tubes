module Main where

import Control.Monad.Random
import Data.Maybe (fromMaybe)
import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, newStdGen)

import Tubes

data GameState = GameState
  { gameTube    :: Tube
  , gameAction  :: Maybe IncompleteAction
  , gamePointer :: Maybe (Float, Float)
  , gameGen     :: StdGen
  }

initGameState :: StdGen -> GameState
initGameState g = GameState
  { gameTube    = initTube
  , gameAction  = Nothing
  , gamePointer = Nothing
  , gameGen     = g
  }

startGameAction :: (Float, Float) -> GameState -> GameState
startGameAction point gs = gs { gameAction = startAction point (gameTube gs) }

updateGamePointer :: (Float, Float) -> GameState -> GameState
updateGamePointer point gs = gs { gamePointer = Just point }

completeGameAction :: (Float, Float) -> GameState -> GameState
completeGameAction point gs = gs
  { gameTube = newTube
  , gameAction = Nothing
  }
  where
    tube = gameTube gs
    newTube = fromMaybe tube $ do
      ia <- gameAction gs
      ca <- completeAction point ia tube
      return (handleAction ca tube)

main :: IO ()
main = do
  g <- newStdGen
  play display bgColor fps (initialWorld g) renderWorld handleWorld updateWorld
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = backgroundColor
    fps     = 60

    initialWorld g = (initGameState newGen) { gameTube = newTube }
      where
        (newTube, newGen) = runRand (initRandomTube 3) g

    renderWorld gs
      =  renderPotentialAction (gameAction gs) (gamePointer gs) (gameTube gs)
      <> renderTube (gameTube gs)

    handleWorld (EventKey (MouseButton LeftButton) Down _ point) = startGameAction point
    handleWorld (EventKey (MouseButton LeftButton) Up _ point) = completeGameAction point
    handleWorld (EventMotion point) = updateGamePointer point
    handleWorld _ = id

    updateWorld dt gs = gs { gameTube = newTube, gameGen = newGen }
      where
        (newTube, newGen) = runRand (updateTube (gameSpeed * dt) (gameTube gs)) (gameGen gs)

    winSize = (800, 450)
    winOffset = (100, 100)

