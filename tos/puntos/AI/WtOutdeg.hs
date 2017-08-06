{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module AI.WtOutdeg
  (ai) where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Semigroup
import System.Random

import Graph.UnionFind
import qualified Protocol as P
import Protocol.Ext (riversFromMove)

type MyState = (
  P.PunterId,
  [Int],
  [(Int, Int)],
  [Int],
  [(Int, Int)])

data MyScore = MyScore {
  cntDegree :: Int,
  cntMine :: Int}

instance Semigroup MyScore where
  MyScore n1 m1 <> MyScore n2 m2 = MyScore (n1+n2) (m1+m2)

ai :: P.Punter (StateT MyState IO)
ai (P.QueryInit punter punters map_) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = concat [[(s, t), (t, s)] | P.River s t <- P.rivers map_]
    mines = P.mines map_
    myEs = []
  put (punter, vs, es, mines, myEs)
  return $ P.AnswerReady punter

ai (P.QueryMove moves) = do
  (punter, vs, esOld, mines, myEs) <- get
  let
    es = removeClaimed moves esOld
    degs = degrees vs es
    setMines = S.fromList mines
    isMine = flip S.member setMines
  
  esWithScore :: [((Int, Int), Int)] <- runUnionFindT $ do
    forM_ vs $ \ v ->
      ufFresh v $ MyScore {
        cntDegree=degs v,
        cntMine=(if isMine v then 1 else 0)}
    forM_ myEs $ \ (s, t) -> ufUnify s t
    forM es $ \ e@(s, t) -> do
      (c1, MyScore n1 m1) <- ufGet s
      (c2, MyScore n2 m2) <- ufGet t
      return (e,
        if c1 == c2
        then -1
        else (n1+n2)*(m1+m2) - n1*m1 - n2*m2
        )

  let
    ans = map fst . reverse $ sortOn snd esWithScore

  case ans of
    [] -> return $ P.AnswerMove $ P.MovePass punter
    ((s,t):_) -> do
      put (punter, vs, es, mines,
        (s,t) : myEs)
      return $ P.AnswerMove $ P.MoveClaim punter s t

ai (P.QueryStop mvs scores) = do
  return P.AnswerNothing


removeClaimed moves es = es \\ cs
  where
    cs = concat [[(s, t), (t, s)] | P.River s t <- concatMap riversFromMove moves]

degrees vs es = let
  zero = M.fromList [(v, 0) | v <- vs]
  add1 x = M.insertWith (+) x 1
  m = foldr ($) zero $ [add1 s | (s, t) <- es]
  in (m M.!)
