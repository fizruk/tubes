module Tubes.Bot where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, modifyTVar)
import Control.Monad (forever, void)

import Tubes.Model

type Bot = PlayerId -> Universe -> Maybe CompleteAction

newLineBot :: Bot
newLineBot _botId u = case map stationLocation (tubeStations (universeTube u)) of
  (to:from:_)
    | null (findSegments from to (universeTube u)) -> Just (StartNewLine from (Present to))
  _ -> Nothing

extendLineBot :: Bot
extendLineBot _botId u
  = case concatMap tubeLineStations (tubeLines (universeTube u)) of
      (from:_) -> case map stationLocation (tubeStations (universeTube u)) of
        (to:_)
          | null (findSegments from to (universeTube u)) -> Just (ContinueLine 0 Backward from (Present to))
        _ -> Nothing
      _ -> Nothing

-- | Add a bot to the 'Universe'.
spawnBot :: PlayerId -> Bot -> TVar Universe -> IO ()
spawnBot botId bot w = do
  atomically $ modifyTVar w (addPlayer botId)
  void $ forkIO $ forever $ do
    threadDelay sec
    atomically $ do
      u <- readTVar w
      case bot botId u of
        Just action -> writeTVar w (applyPlayerCompleteAction botId action u)
        Nothing -> return ()
  where
    sec = 10^6  -- one second in milliseconds
