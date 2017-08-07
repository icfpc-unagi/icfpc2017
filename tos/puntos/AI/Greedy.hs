{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.Greedy
  (ai) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import Data.Tuple

import qualified Graph.Adj as G
import Graph.Dijkstra (dijkstra1)
import Graph.UnionFind
import Lib.EPrint
import Lib.Random

import qualified Protocol as P
-- import Protocol.Ext (riversFromMove)

type Edge = (Int, Int)
type MyState = (ROState, M.Map Edge Int, [Edge], Int)

type ROState = (P.PunterId, DistTable, Bool, [Int])
type DistTable = M.Map P.SiteId (M.Map P.SiteId Int)


data MyWt = MyWt { cMines, cSites :: S.Set P.SiteId }
instance Semigroup MyWt where
  MyWt m1 s1 <> MyWt m2 s2 = MyWt (S.union m1 m2) (S.union s1 s2)


ai :: P.Punter (StateT MyState IO)

ai (P.QueryInit punter punters map_ settings) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = [(s, t) | P.River s t <- P.rivers map_]
    mines = P.mines map_
    opCnt = length $ P.mines map_
  let
    g = G.buildUndir vs es
    adj v = S.toList $ G.edges g M.! v
    distTable = M.fromList [(v0, dijkstra1 v0 adj) | v0 <- mines]
  -- forM mines $ \ v0 -> liftIO $ eprint $ dijkstra1 v0 adj

  put ((punter, distTable, P.options settings, vs),
    M.fromList [(e, 2) | e <- es], [], opCnt)
  return $ P.AnswerReady punter


ai (P.QueryMove moves) = do
  (readOnly, esOld, myEs, opCnt) <- get
  -- liftIO $ eprint opCnt
  let
    (punter, distTable, flagOptions, vs) = readOnly
    es = removeClaimed punter moves esOld
  put (readOnly, es, myEs, opCnt)

  let
    mines = M.keysSet distTable
    esAvail = M.keys $
      if opCnt >= 1 && flagOptions then es else M.filter (== 2) es

  esWithScore :: [(Edge, Int)] <- runUnionFindT $ do
    forM_ vs $ \ v ->
      ufFresh v $ MyWt {
        cMines=if S.member v mines then S.singleton v else S.empty,
        cSites=S.singleton v }
    forM_ myEs $ \ (s, t) -> ufUnify s t

    forM esAvail $ \ e@(s, t) -> do
      (c1, w1) <- ufGet s
      (c2, w2) <- ufGet t
      return (e,
        if c1 == c2
        then -1
        else scoreIncr distTable w1 w2
        )

  let
    ans = map fst . reverse $ sortOn snd esWithScore

  case ans of
    [] -> return $ P.AnswerMove $ P.MovePass punter
    (e:_) -> aiClaimOrOpt e

ai (P.QueryStop mvs scores) = do
  -- TODO: Check my score
  return P.AnswerNothing

scoreIncr :: DistTable -> MyWt -> MyWt -> Int
scoreIncr distTable (MyWt m1 s1) (MyWt m2 s2) = f m1 s2 + f m2 s1
  where
    f ms ss = sum [(distTable M.! m M.! s)^2 | m <- S.toList ms, s <- S.toList ss]

needOpt es e = let
  [n] = catMaybes [M.lookup e es, M.lookup (swap e) es]
  in n == 1

aiClaimOrOpt e = do
  let (s, t) = e
  (readOnly, es, myEs, opCnt) <- get
  let (punter, _,_,_) = readOnly
  if needOpt es e
  then do
    put (readOnly, es, e : myEs, opCnt - 1)
    return $ P.AnswerMove $ P.MoveOption punter s t
  else do
    put (readOnly, es, e : myEs, opCnt)
    return $ P.AnswerMove $ P.MoveClaim punter s t

{-
aiClaim = do
  (punter, vs, es, passCnt, opCnt) <- get
  (s, t) <- liftIO $ randomChoice $ M.keys . M.filter (== 2) $ es
  return $ P.AnswerMove $ P.MoveClaim punter s t
-}

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
