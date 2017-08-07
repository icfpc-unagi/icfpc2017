import Control.Applicative
import Control.Monad
import Data.Monoid
import Options.Applicative

import qualified AI.Rand1 (randAI1)
import qualified AI.Deg (ai)
import qualified AI.Outdeg (ai)
import qualified AI.WtOutdeg (ai)
import qualified AI.Splurand
import qualified AI.Splurand2
import qualified AI.OpSpRand
import qualified AI.Greedy
import qualified AI.Pass

import Handshake (handshake)
import Offline (offline)

data Args = Args {
  aiStr :: String,
  name :: String,
  flagHandshake :: Bool
  }

args = Args 
  <$> strOption (long "ai" <> short 'a' <> metavar "AI")
  <*> strOption (long "name" <> short 'n' <> metavar "NAME" <> value "pUntos")
  <*> switch (long "handshake" <> short 'H')

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Run AI"
     <> header "" )

run :: Args -> IO ()
run (Args aiStr name flagHandshake) = do
  when flagHandshake $
    handshake name
  let
    ai = lookup aiStr $ [
      ("rand1", offline AI.Rand1.randAI1),
      ("deg", offline AI.Deg.ai),
      ("outdeg", offline AI.Outdeg.ai),
      ("wtoutdeg", offline AI.WtOutdeg.ai),
      ("splurand", offline AI.Splurand.ai),
      ("splurand2", offline AI.Splurand2.ai), -- pass ha tamerarenakatta.
      ("opsprand", offline AI.OpSpRand.ai),
      ("greedy", offline AI.Greedy.ai),
      ("pass", offline AI.Pass.ai)
      ]
  maybe (error $ "not found AI name: " ++ aiStr) id ai
