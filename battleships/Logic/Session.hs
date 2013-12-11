{-# LANGUAGE OverloadedStrings #-}
module Logic.Session
  ( serializeToSession
  , deserializeFromSession
  , readFromSession
  , readFromSessionDefault
  , writeToSession
  , deleteFromSession 

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

readFromSession :: (MonadHandler m, Serialize a) => Text -> m (Maybe a)
readFromSession objectKey = do
  serializedObject <- lookupSessionBS objectKey
  return $ case serializedObject of
    Nothing -> Nothing
    Just s -> either (const Nothing) Just $ decode s

readFromSessionDefault :: (MonadHandler m, Serialize a) => Text -> a -> m a
readFromSessionDefault objectKey defaultObject = do
  serializedObject <- lookupSessionBS objectKey
  return $ case serializedObject of
    Nothing -> defaultObject
    Just s -> either (const defaultObject) id $ decode s
    

writeToSession :: (MonadHandler m, Serialize a) => Text -> a -> m ()
writeToSession = serializeToSession

deleteFromSession :: (MonadHandler m) => Text -> m ()
deleteFromSession objectKey = deleteSession objectKey
