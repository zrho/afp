{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.ResetShips (postResetShipsR) where

import Import
import qualified Data.Text  as T
import Data.Serialize
import Logic.GameFramework
import Data.List as L
import Data.ByteString as BS
import Data.Maybe

import Handler.PlaceShipsHelper
import Logic.Session

postResetShipsR :: Handler Html
postResetShipsR = do
  writeToSession fleetPendingKey ([] :: Fleet)
  redirect PlaceShipsR
