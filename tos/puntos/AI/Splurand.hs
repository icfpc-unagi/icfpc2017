{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.Splurand
  (ai) where

import System.Random

import qualified Graph.Adj as G

import qualified Protocol as P
import Protocol.Ext (riversFromMove)

type MyState = (P.PunterId, [Int], [(Int, Int)], Int)

ai :: P.Punter (StateT MyState IO)

ai (P.QueryInit punter punters map_) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = [(s, t) | P.River s t <- P.rivers map_]
  put (punter, vs, es, 0)
  return $ P.AnswerReady punter

ai (P.QueryMove moves) = do
  (punter, vs, esOld, passCnt) <- get
  let
    es = removeClaimed moves esOld

  if passCnt == 0
  then do
    rndP <- liftIO $ randomRIO (0.0, 1.0)
    if rndP < 0.3
    then do
      put (punter, vs, es, passCnt + 1)
      return $ P.AnswerMove $ P.MovePass punter
    else do
      {-
      rndIx <- liftIO $ randomRIO (0, length es - 1)
      let
        (s, t) = es !! rndIx
      -}
      (s, t) <- randomChoice es
      return $ P.AnswerMove $ P.MoveClaim punter s t
  else do
    let
      g = G.buildUndir vs es
      degs = M.map length $ G.edges g
      goodVs = M.keys $ M.filter (>= 2) degs

    r1 <- randomChoice goodVs
    [r0, r2] <- randomSample $ S.toList $ (G.edges g) M.! r1
    aaa

removeClaimed moves es = es \\ cs
  where
    cs = concat [[(s, t), (t, s)] | P.River s t <- concatMap riversFromMove moves]
