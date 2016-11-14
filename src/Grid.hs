{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Grid (Coords, distanceI, distanceF) where

type Coords = (Int, Int)

distanceI :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
distanceI a b = sqrt $ fromIntegral $ distanceSq a b

distanceF :: Floating a => (a, a) -> (a, a) -> a
distanceF a b = sqrt $ distanceSq a b

distanceSq :: Num a => (a, a) -> (a, a) -> a
distanceSq (x1, y1) (x2, y2) = (y2 - y1) ^ 2 + (x2 - x1) ^ 2