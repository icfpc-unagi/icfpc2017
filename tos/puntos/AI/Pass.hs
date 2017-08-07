module AI.Pass
  (ai) where

import Control.Monad.Trans.State
-- import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified Protocol as P

-- type MyState = (P.PunterId, BL.ByteString)
type MyState = (P.PunterId, T.Text)

-- gomi = BL.fromStrict $ C.replicate 1000000 'U'
gomi = T.replicate 1000000 $ T.pack "U"

ai (P.QueryInit punter _ _ _) = do
  put (punter, gomi)
  return $ P.AnswerReady punter

ai (P.QueryMove _) = do
  (punter, gomi) <- get
  return $ P.AnswerMove $ P.MovePass punter

ai (P.QueryStop _ _) = do
  return P.AnswerNothing
