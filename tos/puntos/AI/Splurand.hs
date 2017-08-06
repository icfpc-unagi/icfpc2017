{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.Splurand
  (ai) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Graph.Adj as G
import Lib.Random

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
    flag <- liftIO $ withProb 0.3
    if flag
    then do
      put (punter, vs, es, passCnt + 1)
      return $ P.AnswerMove $ P.MovePass punter
    else do
      put (punter, vs, es, passCnt)
      {-
      rndIx <- liftIO $ randomRIO (0, length es - 1)
      let
        (s, t) = es !! rndIx
      -}
      (s, t) <- liftIO $ randomChoice es
      return $ P.AnswerMove $ P.MoveClaim punter s t
  else do
    put (punter, vs, es, 0)
    let
      g = G.buildUndir vs es
      degs = M.map length $ G.edges g
      goodVs = M.keys $ M.filter (>= 2) degs

    r1 <- liftIO $ randomChoice goodVs
    [r0, r2] <- liftIO $ randomSample 2 $ S.toList $ (G.edges g) M.! r1
    return $ P.AnswerMove $ P.MoveSplurge punter [r0, r1, r2]

removeClaimed moves es = es \\ cs
  where
    cs = concat [[(s, t), (t, s)] | P.River s t <- concatMap riversFromMove moves]
