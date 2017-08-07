module Lib.EPrint
  where

import System.IO

eprint x =
  hPutStrLn stderr (show x)
