module Lib.Random
  where

import System.Random

withProb :: Double -> IO Bool
withProb p = (< p) <$> randomRIO (0, 1)

randomChoice :: [a] -> IO a
randomChoice xs =
  (xs !!) <$> randomRIO (0, length xs - 1)

randomSample :: Int -> [a] -> IO [a]
randomSample 0 xs = return []
randomSample n xs = do
  i <- randomRIO (0, length xs - 1)
  let (ls, x : rs) = splitAt i xs
  (x :) <$> randomSample (n - 1) (ls ++ rs)
