{-# LANGUAGE OverloadedStrings #-}

module Handshake
  where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson

import qualified NColon

type Name = String

data Me = Me {me :: Name}
  deriving Show
instance ToJSON Me where
  toJSON (Me name) = object ["me" .= name]

data You = You {you :: Name}
  deriving Show
instance FromJSON You where
  parseJSON = withObject "You" $ \ v -> You
    <$> v .: "you"


handshake :: (MonadIO m) => Name -> m ()
handshake name = liftIO $ do
  NColon.put $ encode $ Me name
  You name' <- either fail return . eitherDecode =<< NColon.get
  when (name /= name') $
    fail "handshake failed"
