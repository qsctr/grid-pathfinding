module Main where

import Console
import Draw
import Graphics.Gloss.Interface.IO.Game
import Listener
import Search
import State

main :: IO ()
main = do
  initConsole
  playIO (InWindow "Grid pathfinding" (800, 600) (500, 0)) black 60 initState draw listener $ const update