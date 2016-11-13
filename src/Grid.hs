{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Grid (Coords, GridType (..), adjacent, nearest, distance, pictureOf, mapPair) where

import Data.Function
import Graphics.Gloss

type Coords = (Int, Int)

data GridType = Square | Hexagon

adjacent :: GridType -> Coords -> [Coords]
adjacent Square (x, y) =
  [ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  , (x - 1, y    ),             (x + 1, y    )
  , (x - 1, y - 1), (x, y - 1), (x + 1, y - 1) ]
adjacent Hexagon (x, y) =
  [           (x, y + 4)
  , (x - 3, y + 2), (x + 3, y + 2)
  , (x - 3, y - 2), (x + 3, y - 2)
  ,           (x, y - 4)           ]

nearest :: GridType -> Point -> Coords
nearest Square point = mapPair round point
nearest Hexagon (x, y)
  | (x' `mod` 6 == 0) == (y' `mod` 4 == 0) = closer (x', y') (x' + 3, y' + 2)
  | otherwise = closer (x', y' + 2) (x' + 3, y')
  where (x', y') = (floor (x / 3) * 3, floor (y / 2) * 2)
        closer a b
          | ((<) `on` (distanceFloat (x, y) . mapPair fromIntegral)) a b = a
          | otherwise = b

distance :: Coords -> Coords -> Double
distance a b = sqrt $ fromIntegral $ distanceSquared a b

distanceFloat :: Point -> Point -> Float
distanceFloat a b = sqrt $ distanceSquared a b

distanceSquared :: (Num a) => (a, a) -> (a, a) -> a
distanceSquared (x1, y1) (x2, y2) = (y2 - y1) ^ 2 + (x2 - x1) ^ 2

pictureOf :: GridType -> Picture
pictureOf Square = rectangleSolid 1 1
pictureOf Hexagon = Polygon [(1, 2), (2, 0), (1, -2), (-1, -2), (-2, 0), (-1, 2)]

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)