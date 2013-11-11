{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Start (getStartR) where

import Import
import Handler.Helper
import GameLogic.TicTacToe
import GameLogic.Search
import GameLogic.Interaction
import Data.Maybe (fromJust)

getStartR :: Handler Html
getStartR = defaultLayout $ do
  makeHeader
  [whamlet|
    <p>
      <a href=@{GameR computerStartField}>Computer, you start!
  |]
  makeTicTacToeField True initialField
  makeFooter where
    computerStartField = computerMove initialField
