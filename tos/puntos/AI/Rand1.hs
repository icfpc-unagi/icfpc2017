{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module AI.Rand1
  (randAI1) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Maybe
import System.Random

import Protocol

data MyState = MyState {
  myid :: PunterId,
  availableRivers :: [River]
  }
  deriving Show
instance FromJSON MyState where
  parseJSON = withObject "aaa" $ \ v -> MyState
    <$> v .: "p"
    <*> v .: "r"
instance ToJSON MyState where
  toJSON (MyState p r) = object ["p" .= p, "r" .= r]

randAI1 :: Punter (StateT MyState IO)
randAI1 = Punter iP sP
  where
    iP (InitData punter punters map_) = do
      let
        p = punter
        r = rivers map_
      put $ MyState p r

    sP (QueryMove mvs) = do
      MyState p rOld <- get
      let
        rClaimed = catMaybes $ map riverFromClaim mvs
        rNew = rOld \\ rClaimed
        k = length rNew
      put $ MyState p rNew
      if k == 0
      then
        return $ MovePass p
      else do
        ix <- liftIO $ randomRIO (0, k-1)
        let
          River s t = rNew !! ix
        return $ MoveClaim p s t

    sP (QueryStop mvs scores) = do
      liftIO $ print scores
      return undefined

riverFromClaim (MoveClaim p s t) = Just $ River s t
riverFromClaim _ = Nothing
