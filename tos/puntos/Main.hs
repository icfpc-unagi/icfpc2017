{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  DuplicateRecordFields #-}

import Control.Monad
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

{-
type InitData = JSON
type PlayData = JSON
type MoveData = JSON
type StopData = JSON
-}

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

data PlayData = PlayData {
  move :: Moves
  -- , state :: State_
  }
  deriving Show
instance FromJSON PlayData where
  parseJSON = withObject "__play__" $ \ v -> PlayData
    <$> v .: "move"
    -- <*> v .: "state"

data Moves = Moves {moves :: [Move]}
  deriving Show
instance FromJSON Moves where
  parseJSON = withObject "Moves" $ \ v -> Moves
    <$> v .: "moves"

{-
data Move = Either MovePass MoveClaim

data MoveClaim = MoveClaim {
  punter :: PunterId,
  source :: SiteId,
  target :: SiteId
  }
  deriving Show
instance FromJSON MoveClaim where
  parseJSON = withObject "Move(C)" $ \ v -> MoveClaim
    <$> v .: "punter"
    <*> v .: "source"
    <*> v .: "target"

data MovePass = MovePass {punter :: PunterId}
  deriving Show
instance FromJSON MovePass where
  parseJSON = withObject "Move(P)" $ \ v -> MovePass
    <$> v .: "punter"
-}

data Move =
    MoveClaim {
      punter :: PunterId,
      source :: SiteId,
      target :: SiteId
      }
  | MovePass {punter :: PunterId}
  deriving Show
instance FromJSON Move where
  parseJSON = withObject "Move" $ \ v -> (do
      w <- v .: "claim"
      let parseJSONClaim = withObject "Claim" $ \ w -> MoveClaim
            <$> w .: "punter"
            <*> w .: "source"
            <*> w .: "target"
      parseJSONClaim w
    ) <|> (do
      w <- v .: "pass"
      let parseJSONPass = withObject "Pass" $ \ w -> MovePass
            <$> w .: "punter"
      parseJSONPass w
    )
{-
  parseJSON = withObject "Move" $ \ v ->
    (MoveClaim
      <$> v .: "claim" .: "punter"
      <*> v .: "claim" .: "source"
      <*> v .: "claim" .: "target"
    )
    <|>
    (MovePass <$> v .: "pass" .: "punter")
-}


data StopData = StopData {
  moves :: [Move],
  scores :: [Score]
  }
  deriving Show
instance FromJSON StopData where
  parseJSON = withObject "__stop__" $ \ v -> StopData
    <$> v .: "moves"
    <*> v .: "scores"

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
  replicateM_ 3 $ do
    y :: PlayData <- dbgDecode =<< BL.fromStrict <$> B.getLine
    print y
  z :: StopData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  print z
  


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
