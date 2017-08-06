import Control.Applicative
import Data.Monoid
import Options.Applicative

import qualified AI.Rand1 (randAI1)
import qualified AI.Deg (ai)
import qualified AI.Outdeg (ai)
import qualified AI.WtOutdeg (ai)
import qualified AI.Splurand

import Offline (offline)

data Args = Args {
  aiStr :: String
  }

args = Args 
  <$> strOption (long "ai" <> short 'a' <> metavar "AI")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Run AI"
     <> header "" )

run :: Args -> IO ()
run (Args aiStr) = do
  let
    ai = lookup aiStr $ [
      ("rand1", offline AI.Rand1.randAI1),
      ("deg", offline AI.Deg.ai),
      ("outdeg", offline AI.Outdeg.ai),
      ("wtoutdeg", offline AI.WtOutdeg.ai),
      ("splurand", offline AI.Splurand.ai)
      ]
  maybe (error $ "not found AI name: " ++ aiStr) id ai

