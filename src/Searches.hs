{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Searches (search, SearchType (..)) where

import Data.Function
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as Set

type Coords = (Int, Int)
type SearchInfo = (SearchType, Coords, Coords, Set.Set Node)
type SearchData = (PQ.MinQueue PQNode, Set.Set Node)
type SearchResult = Either (Coords, [Coords], SearchData) (Maybe [Coords])

data Node = Node { coords :: Coords, score :: Double, parent :: Maybe Node }
instance Eq Node where (==) = (==) `on` coords
instance Ord Node where compare = compare `on` coords

newtype PQNode = PQNode { unPQNode :: Node }
instance Eq PQNode where (==) = (==) `on` score . unPQNode
instance Ord PQNode where compare = compare `on` score . unPQNode

data SearchType = UniformCost | Greedy | AStar

search :: SearchInfo -> Maybe SearchData -> SearchResult
search info@(searchType, start, goal, walls) searchData = case searchData of
  Nothing ->
    let initScore = case searchType of
          UniformCost -> 0
          _ -> distance start goal
    in  search info $ Just (PQ.singleton $ PQNode $ Node start initScore Nothing, Set.empty)
  Just (fringe, expanded) -> case PQ.minView fringe of
    Nothing -> Right Nothing
    Just ((PQNode current), fringe') ->
      if Set.member current expanded then
          search info $ Just (fringe', expanded)
      else if coords current == goal then
        let buildPath prev path = case parent prev of
              Just node -> buildPath node $ coords prev : path
              Nothing -> coords prev : path
        in  Right $ Just $ buildPath current []
      else
        let childNodes = [ child | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)
              , let childCoords = (fst (coords current) + a, snd (coords current) + b)
                    childScore = case searchType of
                      UniformCost -> uniformCost
                      Greedy -> greedy
                      AStar -> uniformCost + greedy
                    uniformCost = score current + distance childCoords (coords current)
                    greedy = distance childCoords goal
                    child = Node childCoords childScore $ Just current
              , Set.notMember child walls, Set.notMember child expanded' ]
            fringe'' = PQ.union fringe' $ PQ.fromList $ map PQNode childNodes
            expanded' = Set.insert current expanded
        in  Left (coords current, map coords childNodes, (fringe'', expanded'))

distance :: Coords -> Coords -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (y2 - y1) ^ 2 + (x2 - x1) ^ 2