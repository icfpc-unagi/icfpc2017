{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  DuplicateRecordFields #-}

import Control.Monad
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

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
    MoveData {
      moves :: [Move]
      -- , state :: State_
      }
  | StopData {
      moves :: [Move],
      scores :: [Score]
      }
  deriving Show
instance FromJSON StepData where
  parseJSON = withObject "__step__" $ \ v ->
    (v .: "move" >>=
      (withObject "__move__" $ \ w -> MoveData
        <$> w .: "moves")
    )
    <|>
    (v .: "stop" >>=
      (withObject "__stop__" $ \ w -> StopData
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

dbgDecode :: (FromJSON a) => BL.ByteString -> IO a
dbgDecode = either fail return . eitherDecode

main = do
  x :: InitData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  print x
  replicateM_ 4 $ do
    y :: StepData <- dbgDecode =<< BL.fromStrict <$> B.getLine
    print y
  


{-
data AI m = AI {
  initAI :: InitData -> m (),
  playAI :: PlayData -> m MoveData,
  stopAI :: StopData -> m ()
  }
-}



{-
  encodeState :: s -> JSON,
  decodeState :: JSON -> s
-}

{-
data MyState = MyState {}

randomAI :: AI IO
randomAI = AI {
  initAI = 
-}
