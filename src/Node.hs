module Node (Node (..), PQNode (..)) where

import Data.Function
import Data.Ord
import Grid

data Node = Node { coords :: Coords, score :: Double, parent :: Maybe Node }
instance Eq Node where (==) = (==) `on` coords
instance Ord Node where compare = comparing coords

newtype PQNode = PQNode { unPQNode :: Node }
instance Eq PQNode where (==) = (==) `on` score . unPQNode
instance Ord PQNode where compare = comparing (score . unPQNode)