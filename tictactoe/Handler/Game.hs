{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Field
import Handler.Helper
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- * GET and POST handler

getGameR :: TicTacToe -> Handler Html
getGameR f = do
  (formWidget, enctype) <- generateFormPost fieldNumberForm
  defaultLayout $ do
    setDefaultTitle
    $(widgetFile "restart")
    let interactive = True
    $(widgetFile "field")
    $(widgetFile "noscript")

postGameR :: TicTacToe -> Handler Html
postGameR f = do
  (px, py) <- runInputPost positionForm -- get info about move and field
  let maybePos = posInPicture f (px, py)
  handleUserMove f maybePos

positionForm :: FormInput Handler (Double, Double)
positionForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

