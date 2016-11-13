module Searches (search, SearchType (..), SearchData) where

import Data.Function
import Data.Ord
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as Set
import Grid

type SearchInfo = (SearchType, GridType, Coords, Coords, Set.Set Coords)
type SearchData = Maybe (PQ.MinQueue PQNode, Set.Set Node)
type SearchResult = Either (Coords, [Coords], SearchData) (Maybe [Coords])

data Node = Node { coords :: Coords, score :: Double, parent :: Maybe Node }
instance Eq Node where (==) = (==) `on` coords
instance Ord Node where compare = comparing coords

newtype PQNode = PQNode { unPQNode :: Node }
instance Eq PQNode where (==) = (==) `on` score . unPQNode
instance Ord PQNode where compare = comparing (score . unPQNode)

data SearchType = UniformCost | Greedy | AStar

search :: SearchInfo -> SearchData -> SearchResult
search info@(searchType, gridType, start, goal, walls) searchData = case searchData of
  Nothing ->
    let initScore = case searchType of
          UniformCost -> 0
          _ -> distance start goal
    in  search info $ Just (PQ.singleton $ PQNode $ Node start initScore Nothing, Set.empty)
  Just (fringe, expanded) -> case PQ.minView fringe of
    Nothing -> Right Nothing
    Just ((PQNode current), fringe')
      | Set.member current expanded ->
          search info $ Just (fringe', expanded)
      | coords current == goal ->
        let buildPath prev path = case parent prev of
              Just node -> buildPath node $ coords prev : path
              Nothing -> coords prev : path
        in  Right $ Just $ buildPath current []
      | otherwise ->
        let childNodes = [ child | childCoords <- adjacent gridType (coords current)
              , let childScore = case searchType of
                      UniformCost -> uniformCost
                      Greedy -> greedy
                      AStar -> uniformCost + greedy
                    uniformCost = score current + distance childCoords (coords current)
                    greedy = distance childCoords goal
                    child = Node childCoords childScore $ Just current
              , Set.notMember childCoords walls, Set.notMember child expanded' ]
            fringe'' = PQ.union fringe' $ PQ.fromList $ map PQNode childNodes
            fringeList = map (coords . unPQNode) $ PQ.toListU fringe''
            expanded' = Set.insert current expanded
        in  Left (coords current, fringeList, Just (fringe'', expanded'))