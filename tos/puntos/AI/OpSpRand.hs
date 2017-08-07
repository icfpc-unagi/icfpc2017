{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.OpSpRand
  (ai) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple

import qualified Graph.Adj as G
-- import Lib.EPrint
import Lib.Random

import qualified Protocol as P
-- import Protocol.Ext (riversFromMove)

type Edge = (Int, Int)
type MyState = (P.PunterId, [Int], M.Map Edge Int, Int, Int)

ai :: P.Punter (StateT MyState IO)

ai (P.QueryInit punter punters map_ settings) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = [(s, t) | P.River s t <- P.rivers map_]
    opCnt = length $ P.mines map_
  put (punter, vs, M.fromList [(e, 2) | e <- es], 0, opCnt)
  return $ P.AnswerReady punter

ai (P.QueryMove moves) = do
  (punter, vs, esOld, passCnt, opCnt) <- get
  -- liftIO $ eprint (passCnt, opCnt)
  let
    es = removeClaimed punter moves esOld
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
      r1 <- liftIO $ randomChoice goodVs
      [r0, r2] <- liftIO $ randomSample 2 $ S.toList $ (G.edges g) M.! r1
      let
        usedOpt = [e |
          e <- [(r0, r1), (r1, r2)],
          needOpt es e]
        opCntNew = opCnt - length usedOpt
      if opCntNew < 0
      then aiClaimOrOpt
      else do
        put (punter, vs, es, 0, opCntNew)
        return $ P.AnswerMove $ P.MoveSplurge punter [r0, r1, r2]

needOpt es e = let
  [n] = catMaybes [M.lookup e es, M.lookup (swap e) es]
  in n == 1

aiClaimOrOpt = do
  (punter, vs, es, passCnt, opCnt) <- get
  if opCnt == 0
  then aiClaim
  else do
    e@(s, t) <- liftIO $ randomChoice $ M.keys es
    if needOpt es e
    then do
      put (punter, vs, es, passCnt, opCnt - 1)
      return $ P.AnswerMove $ P.MoveOption punter s t
    else
      return $ P.AnswerMove $ P.MoveClaim punter s t

aiClaim = do
  (punter, vs, es, passCnt, opCnt) <- get
  (s, t) <- liftIO $ randomChoice $ M.keys . M.filter (== 2) $ es
  return $ P.AnswerMove $ P.MoveClaim punter s t

removeClaimed punter moves es = M.differenceWith subNat es cs
  where
    subNat n m = let
      a = n - m
      in if a <= 0 then Nothing else Just a
    css0 = concatMap (riversCnt punter) moves
    css1 = map (\((s,t),n) -> ((t,s),n)) css0
    cs = M.unionsWith (+) . map (uncurry M.singleton) $ css0 ++ css1
    
riversCnt :: P.PunterId -> P.Move -> [(Edge, Int)]
riversCnt punter (P.MoveClaim p s t) 
  | p == punter  = [((s, t), 2)]
  | otherwise    = [((s, t), 1)]
riversCnt punter (P.MovePass p) = []
riversCnt punter (P.MoveSplurge p r)
  | p == punter  = [((s, t), 2) | (s, t) <- zip r (tail r)]
  | otherwise    = [((s, t), 1) | (s, t) <- zip r (tail r)]
riversCnt punter (P.MoveOption p s t) = [((s, t), 1)]
