{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.ResetShips (postResetShipsR) where

import Import
import Logic.GameFramework

import Logic.Session

postResetShipsR :: Handler Html
postResetShipsR = do
  writeToSession fleetPendingKey ([] :: Fleet)
  redirect PlaceShipsR
