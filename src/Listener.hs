{-# LANGUAGE RecordWildCards #-}

module Listener (listener) where

import Controls
import Data.Function
import Data.Maybe
import qualified Data.Set as Set
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import Grid
import Search
import State

listener :: Event -> State -> State
listener event state@(State {..}) = case event of
  EventKey key Down mods pos
    | any (elem key) (map snd controls), notClick key || shift mods == Down -> updateViewState
    | SpecialKey KeySpace <- key -> toggleSearch
    | Char 'c' <- key -> cancelSearch
    | Started {} <- search -> state
    | MouseButton LeftButton <- key -> (editWall True pos) { leftButton = True }
    | MouseButton RightButton <- key -> (editWall False pos) { rightButton = True }
    | any ((key ==) . Char) "sb" ->
      setSearch (editWall False pos) $ search { startM = setCoords (startM search) pos }
    | any ((key ==) . Char) "gfe" ->
      setSearch (editWall False pos) $ search { goalM = setCoords (goalM search) pos }
    | Char 'w' <- key -> state { walls = Set.empty, wallList = [] }
    | Char '1' <- key -> state { searchType = UniformCost }
    | Char '2' <- key -> state { searchType = Greedy }
    | Char '3' <- key -> state { searchType = AStar }
  EventMotion pos
    | isJust (viewStateTranslateMark viewState)
      || isJust (viewStateScaleMark viewState) -> updateViewState
    | leftButton && not rightButton -> editWall True pos
    | rightButton && not leftButton -> editWall False pos
  EventKey key Up _ _
    | MouseButton LeftButton <- key -> updateViewState { leftButton = False }
    | MouseButton RightButton <- key -> updateViewState { rightButton = False }
  _ -> state
  where notClick key = key /= MouseButton LeftButton && key /= MouseButton RightButton
        updateViewState = state { viewState = updateViewStateWithEvent event viewState }
        toggleSearch = state
          { search = case search of
            NotStarted (Just start) (Just goal) -> Started
              { searchData = initSearch searchType start goal
              , fringeList = []
              , expandedList = []
              , .. }
            Started {..}
              | Searching {..} <- searchData ->
                search { searchData = searchData { active = not active } }
            _ -> search }
        setSearch state' search' = state' { search = search' }
        cancelSearch = state
          { search = case search of
            Started {..} -> NotStarted (Just start) (Just goal)
            x -> x }
        editWall add pos = case search of
          NotStarted {..}
            | posEqualsMaybeCoords pos startM ->
              setSearch state' $ search { startM = Nothing }
            | posEqualsMaybeCoords pos goalM ->
              setSearch state' $ search { goalM = Nothing }
          _ -> state'
          where state' = state { walls = walls', wallList = Set.toList walls' }
                walls' = (if add then Set.insert else Set.delete) (toCoords pos) walls
        setCoords old pos
          | posEqualsMaybeCoords pos old = Nothing
          | otherwise = Just $ toCoords pos
        posEqualsMaybeCoords pos maybeCoords = maybe False (== toCoords pos) maybeCoords
        toCoords pos = findNearest $ invertViewPort (viewStateViewPort viewState) pos
          where findNearest point = case gridType of
                  Square -> mapPair round point
                  Hexagon ->
                    let (x, y) = point
                        (x', y') = (floor (x / 3) * 3, floor (y / 2) * 2)
                        closer a b
                          | ((<) `on` (distanceF (x, y) . mapPair fromIntegral)) a b = a
                          | otherwise = b
                    in  if (x' `mod` 6 == 0) == (y' `mod` 4 == 0)
                      then closer (x', y') (x' + 3, y' + 2)
                      else closer (x', y' + 2) (x' + 3, y')
                  where mapPair f (a, b) = (f a, f b)