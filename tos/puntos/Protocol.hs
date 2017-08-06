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

type Punter m = Query -> m Answer


data Query =
    QueryInit {
      punter :: PunterId,
      punters :: Nat,
      map_ :: Map_
      }
  | QueryMove {
      moves :: [Move]
      }
  | QueryStop {
      moves :: [Move],
      scores :: [Score]
      }
  deriving Show

instance FromJSON Query where
  parseJSON = withObject "Query" $ \ v ->
    (QueryInit
      <$> v .: "punter"
      <*> v .: "punters"
      <*> v .: "map"
    )
    <|>
    (v .: "move" >>=
      (withObject "QueryMove" $ \ w -> QueryMove
        <$> w .: "moves")
    )
    <|>
    (v .: "stop" >>=
      (withObject "QueryStop" $ \ w -> QueryStop
        <$> w .: "moves"
        <*> w .: "scores"
      )
    )


data Answer =
    AnswerReady PunterId
  | AnswerMove Move
  | AnswerNothing
  deriving Show

instance ToJSON Answer where
  toJSON (AnswerReady p) = object ["ready" .= p]
  toJSON (AnswerMove m) = toJSON m
  toJSON AnswerNothing = object []


data Move =
    MoveClaim {
      punter :: PunterId,
      source :: SiteId,
      target :: SiteId
      }
  | MovePass {punter :: PunterId}
  | MoveSplurge {
      punter :: PunterId,
      route :: [SiteId]
    }
  deriving Show

instance FromJSON Move where
  parseJSON = withObject "Move" $ \ v ->
    (v .: "claim" >>=
      (withObject "MoveClaim" $ \ w -> MoveClaim
        <$> w .: "punter"
        <*> w .: "source"
        <*> w .: "target"
      )
    )
    <|>
    (v .: "pass" >>=
      (withObject "MovePass" $ \ w -> MovePass
        <$> w .: "punter"
      )
    )
    <|>
    (v .: "splurge" >>=
      (withObject "MoveSplurge" $ \ w -> MoveSplurge
        <$> w .: "punter"
        <*> w .: "route"
      )
    )

instance ToJSON Move where
  toJSON (MoveClaim p s t) = object ["claim" .= object [
      "punter" .= p,
      "source" .= s,
      "target" .= t]]
  toJSON (MovePass p) = object ["pass" .= object [
      "punter" .= p]]
  toJSON (MoveSplurge p r) = object ["splurge" .= object [
      "punter" .= p,
      "route"  .= r]]


data Score = Score {
  punter :: PunterId,
  score :: Nat
  }
  deriving Show
instance FromJSON Score where
  parseJSON = withObject "Score" $ \ v -> Score
    <$> v .: "punter"
    <*> v .: "score"


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

data Site = Site {id_ :: SiteId}
  deriving (Show, Eq, Ord)
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


type Nat = Int
type PunterId = Nat
type SiteId = Nat
