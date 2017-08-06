module Graph.UnionFind
  where

-- import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))

type UnionFindT v w m a = StateT (M.Map v (UnionFindVal v w)) m a
data UnionFindVal v w = UnionFindVal v w

runUnionFindT :: (Monad m) => UnionFindT v w m a -> m a
runUnionFindT = flip evalStateT $ M.empty

runUnionFind = runIdentity . runUnionFindT

ufFresh :: (Monad m, Ord v) => v -> w -> UnionFindT v w m ()
ufFresh v w = modify $ M.insert v (UnionFindVal v w)

ufGet :: (Monad m, Ord v) => v -> UnionFindT v w m (v, w)
ufGet v = do
  (UnionFindVal pv pw) <- gets (M.! v)
  if v == pv
    then return (v, pw)
    else do
      (c, w) <- ufGet pv
      modify $ M.insert v (UnionFindVal c undefined)
      return (c, w)

ufClass :: (Monad m, Ord v) => v -> UnionFindT v w m v
ufClass v = fst <$> ufGet v

ufWeight :: (Monad m, Ord v) => v -> UnionFindT v w m w
ufWeight v = snd <$> ufGet v

{-
ufWeight :: (Monad m, Ord v) => v -> UnionFindT v w m w
ufWeight v = do
  cv <- ufClass v
  (UnionFindVal _ w) <- gets (M.! cv)
  return w
-}

ufUnify v1 v2 = do
  (c1, w1) <- ufGet v1
  (c2, w2) <- ufGet v2
  if c1 == c2
    then return False
    else do
      modify $ M.insert c1 (UnionFindVal c2 undefined)
      modify $ M.insert c2 (UnionFindVal c2 $ w1 <> w2)
      return True
