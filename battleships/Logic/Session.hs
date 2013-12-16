{-# LANGUAGE OverloadedStrings #-}
module Logic.Session
  ( serializeToSession
  , deserializeFromSession
  , deserializeFromSessionDefault

  , userFleetKey
  , fleetPendingKey
  , shipListKey
  , rulesKey
  ) where

import Prelude
import Import

import Data.Serialize as Bin

userFleetKey :: Text
userFleetKey = "userFleet"

fleetPendingKey :: Text
fleetPendingKey = "fleetPending"

shipListKey :: Text
shipListKey = "shipList"

rulesKey :: Text
rulesKey = "rules"

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

deserializeFromSessionDefault :: (MonadHandler m, Serialize a) => Text -> a -> m a
deserializeFromSessionDefault objectKey defaultObject = 
  either (const defaultObject) id <$> deserializeFromSession objectKey