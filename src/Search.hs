{-# LANGUAGE RecordWildCards #-}

module Search (initSearch, update) where

import Console
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as Set
import Grid
import Node
import State

initSearch :: SearchType -> Coords -> Coords -> SearchData
initSearch searchType start goal = Searching
  { active = True
  , fringe = PQ.singleton $ PQNode $ Node
    { coords = start
    , score = case searchType of
      UniformCost -> 0
      _ -> distanceI start goal
    , parent = Nothing }
  , expanded = Set.empty }

update :: State -> IO State
update state = update' state 0

update' :: State -> Double -> IO State
update' state@(State {..}) count
  | Started {..} <- search, Searching {..} <- searchData, active =
    case PQ.minView fringe of
      Nothing -> do
        setPathCount "No path found"
        setExpandedCount $ show $ Set.size expanded
        setFringeCount $ show $ PQ.size fringe
        return $ setSearchData NoPath $ updateFringeList state
      Just ((PQNode current), fringe')
        | Set.member current expanded ->
          update' (setSearchData (searchData { fringe = fringe' }) state) count
        | coords current == goal -> do
          let buildPath prev path = case parent prev of
                Just node -> buildPath node $ coords prev : path
                Nothing -> coords prev : path
              result = buildPath current []
          setPathCount $ show $ length result
          setExpandedCount $ show $ Set.size expanded
          setFringeCount $ show $ PQ.size fringe
          return $ setSearchData (FoundPath result) $ updateFringeList state
        | otherwise ->
          let adjacent =
                let (x, y) = coords current
                in  case gridType of
                  Square ->
                    [ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                    , (x - 1, y    ),             (x + 1, y    )
                    , (x - 1, y - 1), (x, y - 1), (x + 1, y - 1) ]
                  Hexagon ->
                    [           (x, y + 4)
                    , (x - 3, y + 2), (x + 3, y + 2)
                    , (x - 3, y - 2), (x + 3, y - 2)
                    ,           (x, y - 4)           ]
              fringe'' = PQ.union fringe' $ PQ.fromList
                [ PQNode child
                | childCoords <- adjacent
                , let childScore = case searchType of
                        UniformCost -> uniformCost
                        Greedy -> greedy
                        AStar -> uniformCost + greedy
                      uniformCost = score current + distanceI childCoords (coords current)
                      greedy = distanceI childCoords goal
                      child = Node childCoords childScore $ Just current
                , Set.notMember childCoords walls
                , Set.notMember child expanded' ]
              expanded' = Set.insert current expanded
              search' = search
                { expandedList = coords current : expandedList
                , searchData = searchData
                  { fringe = fringe'', expanded = expanded' } }
          in  if count >= speed
            then do
              setExpandedCount $ show $ Set.size expanded'
              setFringeCount $ show $ PQ.size fringe''
              return $ updateFringeList state
            else update' (state { search = search' }) (count + 1)
  | otherwise = return state

setSearchData :: SearchData -> State -> State
setSearchData searchData' state = state
  { search = (search state) { searchData = searchData' } }

updateFringeList :: State -> State
updateFringeList state = state { search = (search state)
  { fringeList = map (coords . unPQNode) $ PQ.toListU $ fringe $ searchData $ search state } }