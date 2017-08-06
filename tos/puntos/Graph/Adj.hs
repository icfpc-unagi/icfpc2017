module Graph.Adj
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S


newtype Graph v = Graph { edges :: M.Map v (S.Set v) }

vertices = M.keysSet . edges


build vs es = foldr addEdge (buildDiscrete vs) es

buildDiscrete vs = Graph $ M.fromSet (const S.empty) $ S.fromList vs

addEdge (s, t) = Graph .
  M.adjust (S.insert t) s . edges

{-
data Graph v = Graph {
  vertices :: S.Set v,
  edges :: M.Map v (S.Set v)}
  deriving Show

build vs es = Graph {
  vertices = S.fromList vs,
  edges = M.unionsWith S.union [M.singleton s (S.singleton t) | (s, t) <- es]}
-}

buildUndir vs es = build vs $ es ++ map swap es
