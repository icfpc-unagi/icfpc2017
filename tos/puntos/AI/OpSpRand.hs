{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.OpSpRand
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

type Edge = (Int, Int)
type MyState = (P.PunterId, [Int], M.Map Edge Int, Int, Int)

ai :: P.Punter (StateT MyState IO)

ai (P.QueryInit punter punters map_) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = concat [[(s, t), (t, s)] | P.River s t <- P.rivers map_]
    opCnt = length $ P.mines map_
  put (punter, vs, M.fromList [(e, 2) | e <- es], 0, opCnt)
  return $ P.AnswerReady punter

ai (P.QueryMove moves) = do
  (punter, vs, esOld, passCnt, opCnt) <- get
  let
    es = removeClaimed moves esOld
  put (punter, vs, es, passCnt, opCnt)

  if passCnt == 0
  then do
    flag <- liftIO $ withProb 0.3
    if flag
    then do
      put (punter, vs, es, passCnt + 1, opCnt)
      return $ P.AnswerMove $ P.MovePass punter
    else do
      aiClaimOrOpt
  else do
    let
      g = G.buildUndir vs $ M.keys es
      degs = M.map length $ G.edges g
      goodVs = M.keys $ M.filter (>= 2) degs

    if null goodVs
    then do
      aiClaimOrOpt
    else do
      put (punter, vs, es, 0, opCnt)
      r1 <- liftIO $ randomChoice goodVs
      [r0, r2] <- liftIO $ randomSample 2 $ S.toList $ (G.edges g) M.! r1
      return $ P.AnswerMove $ P.MoveSplurge punter [r0, r1, r2]

aiClaimOrOpt = do
  (punter, vs, es, passCnt, opCnt) <- get
  e@(s, t) <- liftIO $ randomChoice $ M.keys es
  if es M.! e == 1
  then do
    put (punter, vs, es, passCnt, opCnt - 1)
    return $ P.AnswerMove $ P.MoveOption punter s t
  else
    return $ P.AnswerMove $ P.MoveClaim punter s t

removeClaimed moves es = M.filter (/= 0) $ M.unionsWith (+) $ es : css
  where
    css = [M.fromList [((s, t), -1), ((t, s), -1)] | P.River s t <- concatMap riversFromMove moves]
