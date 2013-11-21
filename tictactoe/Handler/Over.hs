{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Over (getOverR) where

import Import
import Handler.Field
import GameLogic.TicTacToe
import GameLogic.Interaction

getOverR :: TicTacToe -> Handler Html
getOverR f = defaultLayout $ do
  $(widgetFile "over")
  makeTicTacToeField False f
  [whamlet|
    <embed src="@{SmileyR}" type="image/svg+xml" />
  |]
