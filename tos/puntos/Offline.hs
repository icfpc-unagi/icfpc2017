{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Offline
  (offline)
  where

import Control.Monad.Trans.State
import Data.Aeson
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import Data.Monoid

import qualified NColon

-- offline :: (MonadState s m) => Punter m -> IO ()
offline punter = do
  WithState s0 q <- either fail return . eitherDecode =<< NColon.get
  (a, s1) <- runStateT (punter q) $ maybe undefined id s0
  NColon.put $ encode $ WithState (Just s1) a


data WithState s x = WithState (Maybe s) x

instance (FromJSON s, FromJSON x) => FromJSON (WithState s x) where
  parseJSON = withObject "WithState" $ \ v -> WithState
    <$> v .:? "state"
    <*> parseJSON (Object v)

instance (ToJSON s, ToJSON x) => ToJSON (WithState s x) where
  toJSON (WithState Nothing x) = toJSON x
  toJSON (WithState (Just s) x) = let
    (Object vs) = object ["state" .= s]
    (Object vx) = toJSON x
    in Object (vs <> vx)
