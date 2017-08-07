{-# LANGUAGE OverloadedStrings #-}

module AI.Deg
  (ai) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Random

import Protocol
import Protocol.Ext (riversFromMove)

data MyState = MyState {
  myid :: PunterId,
  sortedRivers :: [River]
  }
  deriving Show
instance FromJSON MyState where
  parseJSON = withObject "aaa" $ \ v -> MyState
    <$> v .: "p"
    <*> v .: "r"
instance ToJSON MyState where
  toJSON (MyState p r) = object ["p" .= p, "r" .= r]


degrees sites rivers = let
  zero = M.fromList [(s, 0) | Site s <- sites]
  add1 x = M.insertWith (+) x 1
  m = foldr ($) zero $ concat [[add1 s, add1 t] | River s t <- rivers]
  in (m M.!)

ai :: Punter (StateT MyState IO)
ai (QueryInit punter punters map_ settings) = do
      let
        p = punter
        r = rivers map_
        d = degrees (sites map_) r
        score (River s t) = (1 + d s) * (1 + d t)
        rSorted = reverse $ sortOn score r
      put $ MyState p rSorted
      return $ AnswerReady p

ai (QueryMove mvs) = do
      MyState p rOld <- get
      let
        rClaimed = concatMap riversFromMove mvs
        rNew = rOld \\ rClaimed
      put $ MyState p rNew
      if null rNew
      then
        return $ AnswerMove $ MovePass p
      else do
        let
          River s t = head rNew
        return $ AnswerMove $ MoveClaim p s t

ai (QueryStop mvs scores) = do
      -- liftIO $ print scores
      return AnswerNothing
