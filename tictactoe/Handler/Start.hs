{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Start (getStartR) where

import Import
import Handler.Field
import GameLogic.TicTacToe
import GameLogic.Search
import GameLogic.Interaction
import Data.Maybe (fromJust)
import Handler.Helper

getStartR :: Handler Html
getStartR = do
  (formWidget, enctype) <- generateFormPost fieldNumberForm
  defaultLayout $ do
    setDefaultTitle
    $(widgetFile "start")
    let interactive = True
    let f = initialField
    $(widgetFile "field")
    $(widgetFile "noscript")
    where
      computerStartField = computerMove initialField
