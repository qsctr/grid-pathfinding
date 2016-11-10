module Searches (ucs, Node (..)) where

import Data.Function
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as Set

type Coords = (Int, Int)

data Node = Node { coords :: Coords, score :: Double, parent :: Maybe Node }
instance Eq Node where (==) = (==) `on` coords
instance Ord Node where compare = compare `on` coords

newtype PQNode = PQNode { unPQNode :: Node }
instance Eq PQNode where (==) = (==) `on` score . unPQNode
instance Ord PQNode where compare = compare `on` score . unPQNode

ucs :: Coords -> Coords -> Set.Set Node -> Maybe [Node]
ucs start goal walls = ucs' (PQ.singleton $ PQNode $ Node start 0 Nothing) Set.empty
  where
    ucs' fringe expanded = case PQ.minView fringe of
      Nothing -> Nothing
      Just ((PQNode current), fringe') ->
        if Set.member current expanded then
          ucs' fringe' expanded
        else if coords current == goal then
          let buildPath path = case parent (head path) of
                Just node -> buildPath $ node : path
                Nothing -> path
          in  Just $ buildPath [current]
        else
          let expanded' = Set.insert current expanded
              fringe'' = PQ.union fringe' $ PQ.fromList
                [ PQNode child | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)
                , let childCoords = (fst (coords current) + a, snd (coords current) + b)
                , let childScore = score current + distance childCoords (coords current)
                , let child = Node childCoords childScore $ Just current
                , Set.notMember child walls, Set.notMember child expanded' ]
          in  ucs' fringe'' expanded'

distance :: Coords -> Coords -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (y2 - y1) ^ 2 + (x2 - x1) ^ 2