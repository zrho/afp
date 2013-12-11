{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.FinishedPlacing (getFinishedPlacingR) where

import Import
import Logic.GameFramework

import Logic.Session

getFinishedPlacingR :: Handler Html
getFinishedPlacingR = do
  fleet <- readFromSessionDefault fleetPendingKey ([] :: Fleet)
  writeToSession userFleetKey fleet -- this is very the finished fleet is stored
  defaultLayout $ do
    setTitle "Finished placing ships!"
    $(widgetFile "finishedplacing")
