{-# LANGUAGE RecordWildCards #-}

module Draw (draw) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import State

draw :: State -> IO Picture
draw State {..} = return $ applyViewPortToPicture (viewStateViewPort viewState) $ Pictures
    $ map (tile white) wallList
    ++ case search of
      NotStarted {..} -> catMaybes [tile red <$> startM, tile green <$> goalM]
      Started {..} ->
        map (tile (dark blue)) expandedList
        ++ map (tile (light azure)) fringeList
        ++ case searchData of
          FoundPath path -> map (tile yellow) path
          _ -> []
        ++ [tile red start, tile green goal]
    where tile col (x, y) = Color col $ Translate (fromIntegral x) (fromIntegral y)
            $ case gridType of
              Square -> rectangleSolid 1 1
              Hexagon -> Polygon [(1, 2), (2, 0), (1, -2), (-1, -2), (-2, 0), (-1, 2)]