module Main where

import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main =
  play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "Tubes" winSize winOffset
    bgColor = black
    fps     = 60

    initialWorld = ()
    renderWorld w = blank
    handleWorld _ w = w
    updateWorld _ w = w

    winSize = (800, 450)
    winOffset = (100, 100)

