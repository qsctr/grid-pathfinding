module State
  ( State (..)
  , Search (..)
  , SearchData (..)
  , SearchType (..)
  , GridType (..)
  ) where

import Data.PQueue.Min (MinQueue)
import Data.Set (Set)
import Graphics.Gloss.Data.ViewState
import Grid
import Node

data State = State
  { search :: Search
  , searchType :: SearchType
  , gridType :: GridType
  , walls :: Set Coords
  , wallList :: [Coords]
  , speed :: Double
  , viewState :: ViewState
  , leftButton :: Bool
  , rightButton :: Bool }

data Search
  = NotStarted
    { startM :: Maybe Coords
    , goalM :: Maybe Coords }
  | Started
    { start :: Coords
    , goal :: Coords
    , searchData :: SearchData
    , fringeList :: [Coords]
    , expandedList :: [Coords] }

data SearchData
  = Searching
    { active :: Bool
    , fringe :: MinQueue PQNode
    , expanded :: Set Node }
  | FoundPath [Coords]
  | NoPath

data SearchType = UniformCost | Greedy | AStar

data GridType = Square | Hexagon