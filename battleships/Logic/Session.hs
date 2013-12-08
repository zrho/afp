module Logic.Session
  ( serializeToSession
  , deserializeFromSession 
  ) where

import Prelude
import Import

import Data.Text
import Data.Serialize as Bin

-- | Allows to store arbitrary serializable data in sessions
serializeToSession :: (MonadHandler m, Serialize a) => Text -> a -> m ()
serializeToSession key value = setSessionBS key (encode value)

-- | Allows to read arbitrary serializable data from sessions
deserializeFromSession :: (MonadHandler m, Serialize a) => Text -> m (Either String a)
deserializeFromSession key = do
  maybeStr <- lookupSessionBS key
  case maybeStr of
    Nothing  -> return (Left "key not found")
    Just str -> return (decode str)