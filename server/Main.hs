module Main where

import Control.Concurrent (forkIO)
import Data.Tagged (unTagged)
import Network.Wai.Handler.Warp (run)

import Tubes.Server

main :: IO ()
main = do
  config <- newConfig
  forkIO $ periodicUpdates 10000 config   -- update Universe every 10 milliseconds
  run 8000 $ unTagged $ server config
