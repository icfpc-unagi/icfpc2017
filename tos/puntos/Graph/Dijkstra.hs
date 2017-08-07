{-# LANGUAGE ViewPatterns #-}

module Graph.Dijkstra
  where

import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
-- import Data.Tree

-- import qualified Graph.Adj as G

dijkstra1 :: (Ord v) => v -> (v -> [v]) -> M.Map v Int
dijkstra1 v0 adj = execState (go $ Q.singleton (v0, 0)) M.empty
  where
    go (Q.viewl -> Q.EmptyL) = return ()
    go (Q.viewl -> (v, d) Q.:< qs) = do
      m <- gets (M.member v)
      if m
      then do
        go qs
      else do
        modify $ M.insert v d
        go $ qs Q.>< Q.fromList [(w, d+1) | w <- adj v]
