{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Protocol

  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import System.Random

type Nat = Int

type PunterId = Nat

data Map_ = Map_ {
  sites :: [Site],
  rivers :: [River],
  mines :: [SiteId]}
  deriving Show
instance FromJSON Map_ where
  parseJSON = withObject "Map" $ \ v -> Map_
    <$> v .: "sites"
    <*> v .: "rivers"
    <*> v .: "mines"

data Site = Site {id :: SiteId}
  deriving Show
instance FromJSON Site where
  parseJSON = withObject "Site" $ \ v -> Site
    <$> v .: "id"

data River = River {source :: SiteId, target :: SiteId}
  deriving Show
instance FromJSON River where
  parseJSON = withObject "River" $ \ v -> River
    <$> v .: "source"
    <*> v .: "target"
instance ToJSON River where
  toJSON (River s t) = object ["source" .= s, "target" .= t]

instance Eq River where
  (River s0 t0) == (River s1 t1) = or [
    s0 == s1 && t0 == t1,
    s0 == t1 && t0 == s1]

type SiteId = Nat

data InitData = InitData {
  punter :: PunterId,
  punters :: Nat,
  map_ :: Map_
  }
  deriving Show
instance FromJSON InitData where
  parseJSON = withObject "__init__" $ \ v -> InitData
    <$> v .: "punter"
    <*> v .: "punters"
    <*> v .: "map"

data StepData =
    QueryMove {
      moves :: [Move]
      }
  | QueryStop {
      moves :: [Move],
      scores :: [Score]
      }
  deriving Show
instance FromJSON StepData where
  parseJSON = withObject "__step__" $ \ v ->
    (v .: "move" >>=
      (withObject "__move__" $ \ w -> QueryMove
        <$> w .: "moves")
    )
    <|>
    (v .: "stop" >>=
      (withObject "__stop__" $ \ w -> QueryStop
        <$> w .: "moves"
        <*> w .: "scores"
      )
    )

data Move =
    MoveClaim {
      punter :: PunterId,
      source :: SiteId,
      target :: SiteId
      }
  | MovePass {punter :: PunterId}
  deriving Show
instance FromJSON Move where
  parseJSON = withObject "Move" $ \ v ->
    (v .: "claim" >>=
      (withObject "Claim" $ \ w -> MoveClaim
        <$> w .: "punter"
        <*> w .: "source"
        <*> w .: "target"
      )
    )
    <|>
    (v .: "pass" >>=
      (withObject "Pass" $ \ w -> MovePass
        <$> w .: "punter"
      )
    )

data Score = Score {
  punter :: PunterId,
  score :: Nat
  }
  deriving Show
instance FromJSON Score where
  parseJSON = withObject "Score" $ \ v -> Score
    <$> v .: "punter"
    <*> v .: "score"

data AnswerReady s = AnswerReady {
  punter :: PunterId,
  state :: s
  }
  deriving Show
instance ToJSON s => ToJSON (AnswerReady s) where
  toJSON (AnswerReady p s) = object ["ready" .= p, "state" .= toJSON s]

data Punter m = Punter {
  initPunter :: InitData -> m (),
  stepPunter :: StepData -> m Move
  }


