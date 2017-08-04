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
import Offline
import AI.Rand1 (randAI1)


dbgDecode :: (FromJSON a) => BL.ByteString -> IO a
dbgDecode = either fail return . eitherDecode

readTest = do
  x :: Query <- dbgDecode =<< BL.fromStrict <$> B.getLine
  print x
  replicateM_ 4 $ do
    y :: Query <- dbgDecode =<< BL.fromStrict <$> B.getLine
    print y


main = punterOfflineTest randAI1 >> return ()

punterOnlineTest punter = flip runStateT undefined $
    replicateM_ 5 $ do
  input :: Query <- liftIO $ dbgDecode =<< BL.fromStrict <$> B.getLine
  liftIO $ print input
  output <- punter input
  liftIO $ print output
  s <- get
  liftIO $ print s
  

punterOfflineTest punter =
  replicateM 5 $ offline punter
{-
punterOfflineTest punter = do
  q :: Query <- dbgDecode =<< BL.fromStrict <$> B.getLine
  (a, s) <- flip runStateT undefined $ punter q
  encodeWithState s a
  let
    (QueryInit p _ _)  = x
    a = AnswerReady p s
    aj = encode a
  BL.putStr aj
  putStrLn ""

  y :: Query <- dbgDecode =<< BL.fromStrict <$> B.getLine
  (mv, s2) <- flip runStateT s $ punter y
  print mv
  print s2
-}
