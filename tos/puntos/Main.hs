{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  DuplicateRecordFields #-}

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


dbgDecode :: (FromJSON a) => BL.ByteString -> IO a
dbgDecode = either fail return . eitherDecode

readTest = do
  x :: InitData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  print x
  replicateM_ 4 $ do
    y :: StepData <- dbgDecode =<< BL.fromStrict <$> B.getLine
    print y


main = punterOfflineTest randomAI >> return ()

punterOnlineTest punter = flip runStateT undefined $ do
  x :: InitData <- liftIO $ dbgDecode =<< BL.fromStrict <$> B.getLine
  liftIO $ print x
  initPunter punter x
  s <- get
  liftIO $ print s
  replicateM_ 4 $ do
    y :: StepData <- liftIO $ dbgDecode =<< BL.fromStrict <$> B.getLine
    liftIO $ print y
    stepPunter punter y
    s <- get
    liftIO $ print s
  
punterOfflineTest punter = do
  x :: InitData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  ((), s) <- flip runStateT undefined $ initPunter punter x
  let
    (InitData p _ _)  = x
    a = AnswerReady p s
    aj = encode a
  BL.putStr aj
  putStrLn ""

  y :: StepData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  (mv, s2) <- flip runStateT s $ stepPunter punter y
  print mv
  print s2

riverFromClaim (MoveClaim p s t) = Just $ River s t
riverFromClaim _ = Nothing

data Punter m = Punter {
  initPunter :: InitData -> m (),
  stepPunter :: StepData -> m Move
  }


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

randomAI :: Punter (StateT MyState IO)
randomAI = Punter iP sP
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



{-
  encodeState :: s -> JSON,
  decodeState :: JSON -> s
-}
