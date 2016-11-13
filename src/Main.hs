{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main where

import Data.Maybe
import qualified Data.Set as Set
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import Grid
import Searches

data Searching = NotSearching | Searching | Paused | Finished deriving (Eq)

data State = State
  { start :: Maybe Coords
  , goal :: Maybe Coords
  , walls :: Set.Set Coords
  , gridType :: GridType
  , searching :: Searching
  , searchType :: SearchType
  , expanded :: [Coords]
  , fringe :: [Coords]
  , path :: Maybe [Coords]
  , searchData :: SearchData
  , viewState :: ViewState
  , leftButton :: Bool
  , rightButton :: Bool }

main :: IO ()
main = play (InWindow "Grid pathfinding" (800, 600) (500, 0))
  black 100 initState draw listener $ const step

initState :: State
initState = State
  { start = Nothing
  , goal = Nothing
  , walls = Set.empty
  , gridType = Hexagon
  , searching = NotSearching
  , searchType = UniformCost
  , expanded = []
  , fringe = []
  , path = Nothing
  , searchData = Nothing
  , viewState = (viewStateInitWithConfig $ map ((map (, Nothing)) <$>) controls)
    { viewStateViewPort = viewPortInit { viewPortScale = 15 } }
  , leftButton = False
  , rightButton = False }

controls :: [(Command, [Key])]
controls =
  [ (CTranslate, [MouseButton LeftButton])
  , (CScale, [MouseButton RightButton])
  , (CBumpZoomOut, [MouseButton WheelUp, SpecialKey KeyPageUp])
  , (CBumpZoomIn, [MouseButton WheelDown, SpecialKey KeyPageDown])
  , (CBumpLeft, [SpecialKey KeyRight])
  , (CBumpRight, [SpecialKey KeyLeft])
  , (CBumpUp, [SpecialKey KeyDown])
  , (CBumpDown, [SpecialKey KeyUp]) ]

draw :: State -> Picture
draw State {..} = applyViewPortToPicture (viewStateViewPort viewState) $ Pictures
  $ map (tile white) (Set.toList walls)
  ++ map (tile (dark blue)) expanded
  ++ map (tile (light azure)) fringe
  ++ maybe [] (map (tile yellow)) path
  ++ catMaybes [tile red <$> start, tile green <$> goal]
  where tile col coords = Color col
          $ uncurry Translate (mapPair fromIntegral coords) $ pictureOf gridType

listener :: Event -> State -> State
listener event state@(State {..}) = case event of
  EventKey key Down mods pos
    | any (elem key) (map snd controls), (key /= MouseButton LeftButton 
    && key /= MouseButton RightButton) || shift mods == Down -> updateViewState
    | key == SpecialKey KeySpace -> state
      { searching = case searching of
        NotSearching
          | isJust start && isJust goal -> Searching
          | otherwise -> NotSearching
        Searching -> Paused
        Paused -> Searching
        Finished -> Finished }
    | key == Char 'c' -> state
      { searching = NotSearching
      , expanded = []
      , fringe = []
      , path = Nothing
      , searchData = Nothing }
    | searching /= NotSearching -> state
    | key == MouseButton LeftButton -> (editWall True pos) { leftButton = True }
    | key == MouseButton RightButton -> (editWall False pos) { rightButton = True }
    | any ((key ==) . Char) "sb" -> (editWall False pos) { start = setCoords start pos }
    | any ((key ==) . Char) "gfe" -> (editWall False pos) { goal = setCoords goal pos }
    | key == Char 'w' -> state { walls = Set.empty }
    | key == Char '1' -> state { searchType = UniformCost }
    | key == Char '2' -> state { searchType = Greedy }
    | key == Char '3' -> state { searchType = AStar }
  EventMotion pos
    | isJust (viewStateTranslateMark viewState)
      || isJust (viewStateScaleMark viewState) -> updateViewState
    | leftButton && not rightButton -> editWall True pos
    | rightButton && not leftButton -> editWall False pos
  EventKey key Up _ _
    | key == MouseButton LeftButton -> updateViewState { leftButton = False }
    | key == MouseButton RightButton -> updateViewState { rightButton = False }
  _ -> state
  where updateViewState = state { viewState = updateViewStateWithEvent event viewState }
        editWall add pos
          | maybeEquals (toCoords pos) start = state' { start = Nothing }
          | maybeEquals (toCoords pos) goal = state' { goal = Nothing }
          | otherwise = state'
          where state' = state
                  { walls = (if add then Set.insert else Set.delete) (toCoords pos) walls }
        toCoords pos = nearest gridType $ invertViewPort (viewStateViewPort viewState) pos
        setCoords old pos
          | maybeEquals (toCoords pos) old = Nothing
          | otherwise = Just $ toCoords pos
        maybeEquals = maybe False . (==)

step :: State -> State
step state@(State {..})
  | searching == Searching =
    case search (searchType, gridType, fromJust start, fromJust goal, walls) searchData of
      Left (newExpanded, fringe', searchData') -> state
        { expanded = newExpanded : expanded, fringe = fringe', searchData = searchData' }
      Right path' -> state { path = path' }
  | otherwise = state