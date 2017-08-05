module NColon

  where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import System.IO (hFlush, stdin, stdout)

put b = do
  B.hPut stdout $ C.pack $ show (BL.length b) ++ ":"
  BL.hPut stdout b
  hFlush stdout

get = do
  n <- read <$> getN
  BL.fromStrict <$> B.hGet stdin n

getN = do
  [c] <- C.unpack <$> B.hGet stdin 1
  if c == ':'
  then return []
  else (c :) <$> getN
