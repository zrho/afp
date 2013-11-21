{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Start (getStartR) where

import Import
import Handler.Field
import GameLogic.TicTacToe
import GameLogic.Search
import GameLogic.Interaction
import Data.Maybe (fromJust)

getStartR :: Handler Html
getStartR = defaultLayout $ do
  $(widgetFile "start")
  makeTicTacToeField True initialField
  where
    computerStartField = computerMove initialField
