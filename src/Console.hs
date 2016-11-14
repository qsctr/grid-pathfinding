{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Console
  ( initConsole
  , setExpandedCount
  , setFringeCount
  , setPathCount
  , setSpeedNumber
  , setGridType
  , setSearchType
  , setSearching
  ) where

import System.Console.ANSI
import System.IO

initConsole :: IO ()
initConsole = do
  clearScreen
  setTitle "Grid pathfinding"
  setSGR [SetColor Foreground Dull Black]
  setSGR [SetColor Background Vivid White]
  putStrLn "Grid pathfinding"
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn ""
  putStrLn "Nodes expanded: "
  putStrLn "Nodes in fringe: "
  putStrLn "Nodes in path: "
  putStrLn "Speed: 1.0"
  putStrLn "Grid type: Square"
  putStrLn "Search type: Uniform cost"
  putStrLn "Searching: Not searching"
  setSGR [Reset]
  putStrLn "Arrow keys or shift + drag: move"
  putStrLn "Page up/down or scroll: zoom in/out"
  putStrLn "Left/right click: add/remove walls"
  putStrLn "s/b: set start/begin point"
  putStrLn "g/f/e: set goal/finish/end point"
  putStrLn "1/2/3: Uniform cost/Greedy/A* search"
  putStrLn "F1 to F7: Set speed from 1 to 64"
  putStrLn "F8: Set speed to infinity"
  putStrLn "Enter: switch between squares/hexagons"
  putStrLn "w: clear all walls"
  putStrLn "Space bar: start/pause search"
  putStrLn "c: cancel/end search"

setExpandedCount = printAt 2 16
setFringeCount = printAt 3 17
setPathCount = printAt 4 15
setSpeedNumber = printAt 5 7
setGridType = printAt 6 11
setSearchType = printAt 7 13
setSearching = printAt 8 11

printAt :: Int -> Int -> String -> IO ()
printAt row col str = do
  setCursorPosition row col
  clearFromCursorToLineEnd
  setSGR [SetColor Foreground Vivid Blue]
  putStr str
  hFlush stdout