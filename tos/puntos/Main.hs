{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  DuplicateRecordFields #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Protocol
import AI.Rand1 (randAI1)


dbgDecode :: (FromJSON a) => BL.ByteString -> IO a
dbgDecode = either fail return . eitherDecode

readTest = do
  x :: InitData <- dbgDecode =<< BL.fromStrict <$> B.getLine
  print x
  replicateM_ 4 $ do
    y :: StepData <- dbgDecode =<< BL.fromStrict <$> B.getLine
    print y


main = punterOfflineTest randAI1 >> return ()

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
