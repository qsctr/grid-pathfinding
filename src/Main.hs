{-# LANGUAGE TupleSections #-}

module Main where

import Controls
import qualified Data.Set as Set
import Draw
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Listener
import Search
import State

main :: IO ()
main = play (InWindow "Grid pathfinding" (800, 600) (500, 0))
  black 60 initState draw listener $ const update

initState :: State
initState = State
  { search = NotStarted { startM = Nothing, goalM = Nothing }
  , searchType = UniformCost
  , gridType = Hexagon
  , walls = Set.empty
  , wallList = []
  , speed = 1 / 0
  , viewState = (viewStateInitWithConfig $ map ((map (, Nothing)) <$>) controls)
    { viewStateViewPort = viewPortInit { viewPortScale = 15 } }
  , leftButton = False
  , rightButton = False }