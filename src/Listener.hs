{-# LANGUAGE RecordWildCards #-}

module Listener (listener) where

import Console
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

listener :: Event -> State -> IO State
listener event state@(State {..}) = case event of
  EventKey key Down mods pos
    | any (elem key) (map snd controls), notClick key || shift mods == Down ->
      return updateViewState
    | SpecialKey KeySpace <- key -> toggleSearch
    | Char 'c' <- key -> cancelSearch
    | SpecialKey KeyF1 <- key -> setSpeed 1
    | SpecialKey KeyF2 <- key -> setSpeed 2
    | SpecialKey KeyF3 <- key -> setSpeed 4
    | SpecialKey KeyF4 <- key -> setSpeed 8
    | SpecialKey KeyF5 <- key -> setSpeed 16
    | SpecialKey KeyF6 <- key -> setSpeed 32
    | SpecialKey KeyF7 <- key -> setSpeed 64
    | SpecialKey KeyF8 <- key -> setSpeed (1 / 0)
    | Started {} <- search -> return state
    | MouseButton LeftButton <- key -> return $ (editWall True pos) { leftButton = True }
    | MouseButton RightButton <- key -> return $ (editWall False pos) { rightButton = True }
    | any ((key ==) . Char) "sb" -> return 
      $ setSearch (editWall False pos) $ search { startM = setCoords (startM search) pos }
    | any ((key ==) . Char) "gfe" -> return
      $ setSearch (editWall False pos) $ search { goalM = setCoords (goalM search) pos }
    | Char 'w' <- key -> return $ state { walls = Set.empty, wallList = [] }
    | Char '1' <- key -> do
      setSearchType "Uniform cost"
      return $ state { searchType = UniformCost }
    | Char '2' <- key -> do
      setSearchType "Greedy"
      return $ state { searchType = Greedy }
    | Char '3' <- key -> do
      setSearchType "A*"
      return $ state { searchType = AStar }
    | SpecialKey KeyEnter <- key -> toggleGridType
  EventMotion pos
    | isJust (viewStateTranslateMark viewState)
      || isJust (viewStateScaleMark viewState) -> return updateViewState
    | leftButton && not rightButton -> return $ editWall True pos
    | rightButton && not leftButton -> return $ editWall False pos
  EventKey key Up _ _
    | MouseButton LeftButton <- key -> return $ updateViewState { leftButton = False }
    | MouseButton RightButton <- key -> return $ updateViewState { rightButton = False }
  _ -> return state
  where notClick key = key /= MouseButton LeftButton && key /= MouseButton RightButton
        updateViewState = state { viewState = updateViewStateWithEvent event viewState }
        toggleSearch = case search of
          NotStarted (Just start) (Just goal) -> do
            setSearching "Searching"
            return $ state { search = Started
              { searchData = initSearch searchType start goal
              , fringeList = []
              , expandedList = []
              , .. } }
          Started {..}
            | Searching {..} <- searchData -> do
              setSearching (if active then "Paused" else "Searching")
              return $ state { search = search
                { searchData = searchData { active = not active } } }
          _ -> return state
        setSearch state' search' = state' { search = search' }
        cancelSearch = case search of
          Started {..} -> do
            setSearching "Not searching"
            setExpandedCount ""
            setFringeCount ""
            setPathCount ""
            return state { search = NotStarted (Just start) (Just goal) }
          _ -> return state
        setSpeed n = do
          setSpeedNumber $ show n
          return $ state { speed = n }
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
        toggleGridType = do
          setGridType $ case gridType of
            Square -> "Hexagon"
            Hexagon -> "Square"
          return $ state
            { search = NotStarted { startM = Nothing, goalM = Nothing }
            , gridType = case gridType of
              Square -> Hexagon
              Hexagon -> Square
            , walls = Set.empty
            , wallList = [] }
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