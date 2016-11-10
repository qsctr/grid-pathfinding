module Main where

import Graphics.Gloss

import qualified Data.Map.Strict       as Map
import qualified Data.PQueue.Prio.Min  as PQ
import qualified Data.Set              as Set

import Data.Tuple

type Score = Double
type Node = (Int, Int)

main :: IO ()
main = do
  putStrLn "hi"
  print $ ucs (0, 0) (99, 204)

walls :: Set.Set Node
walls = Set.empty

ucs :: Node -> Node -> Maybe [Node]
ucs start goal =
  let defFringe = PQ.singleton 0 start
      defExpanded = Set.empty
      defScores = Map.singleton start 0
      defParents = Map.singleton start Nothing
  in  ucs' defFringe defExpanded defScores defParents
  where
    ucs' fringe expanded scores parents = case PQ.minViewWithKey fringe of
      Nothing -> Nothing
      Just ((currScore, currNode), fringe') -> 
        if currNode == goal then
          let buildPath path = case parents Map.! (head path) of
                Just parent -> buildPath $ parent : path
                Nothing -> path
          in  Just $ buildPath [goal]
        else
          let connections = [ (child, score)
                | a <- [-1..1]
                , b <- [-1..1]
                , (a, b) /= (0, 0)
                , let child = (fst currNode + a, snd currNode + b)
                , let score = currScore + distance child currNode
                , Set.notMember child walls
                , Set.notMember child expanded'
                , score < Map.findWithDefault (1 / 0) child scores ]
              fringe'' = PQ.union fringe' $ PQ.fromList $ map swap connections
              expanded' = Set.insert currNode expanded
              scores' = Map.union (Map.fromList connections) scores
              parents' = Map.union (Map.fromList $ map (fmap $ const $ Just currNode) connections) parents
          in  ucs' fringe'' expanded' scores' parents'

distance :: Node -> Node -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (y2 - y1) ^ 2 + (x2 - x1) ^ 2