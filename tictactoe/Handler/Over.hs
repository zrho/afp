{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Over (getOverR) where

import Import
import Handler.Field
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Helper

getOverR :: TicTacToe -> Handler Html
getOverR f = do
  defaultLayout $ do
    setDefaultTitle
    $(widgetFile "over")
    let interactive = False
    $(widgetFile "field")
