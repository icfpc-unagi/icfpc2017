{-# LANGUAGE OverloadedStrings #-}

module AI.Outdeg
  (ai) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
-- import qualified Graph.UnionFind as UF
import System.Random

import qualified Protocol as P
import Protocol.Ext (riversFromMove)

type MyState = (
  P.PunterId,
  [Int],
  [(Int, Int)],
  [Int],
  [(Int, Int)])

ai :: P.Punter (StateT MyState IO)
ai (P.QueryInit punter punters map_ settings) = do
  let
    vs = [i | P.Site i <- P.sites map_]
    es = concat [[(s, t), (t, s)] | P.River s t <- P.rivers map_]
    myvs = P.mines map_
    myes = []
  put (punter, vs, es, myvs, myes)
  return $ P.AnswerReady punter

ai (P.QueryMove moves) = do
  (punter, vs, es0, myvs, myes) <- get
  let
    es1 = removeClaimed moves es0
    degs = degrees vs es1

  let
    setMyvs = S.fromList myvs
    good (s, t) = and [
      S.member s setMyvs,
      not $ S.member t setMyvs]
    goodEs = filter good es1
    ans = reverse $ sortOn (degs . snd) goodEs

  case ans of
    [] -> return $ P.AnswerMove $ P.MovePass punter
    ((s,t):_) -> do
      put (punter, vs, es1,
        t : myvs,
        (s,t) : (t,s) : myes)
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
