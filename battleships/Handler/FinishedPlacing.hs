{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.FinishedPlacing (getFinishedPlacingR) where

import Import
import qualified Data.Text  as T
import Data.Serialize
import Logic.GameFramework
import Data.List as L
import Data.ByteString as BS
import Data.Maybe

import Handler.PlaceShipsHelper
import Logic.Session

getFinishedPlacingR :: Handler Html
getFinishedPlacingR = do
  fleet <- readFromSessionDefault fleetPendingKey ([] :: Fleet)
  writeToSession userFleetKey fleet -- this is very the finished fleet is stored
  defaultLayout $ do
    setTitle "Finished placing ships!"
    $(widgetFile "finishedplacing")
