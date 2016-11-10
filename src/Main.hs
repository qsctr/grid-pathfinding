module Main where

import qualified Data.Set as Set
import Searches

main :: IO ()
main = do
  putStrLn "hello world"
  print $ map coords <$> ucs (0, 0) (99, 204) Set.empty