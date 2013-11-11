{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Field
import Handler.Helper

-------------------------------------------------------------------------------
-- * GET and POST handler

getGameR :: TicTacToe -> Handler Html
getGameR f = defaultLayout $ do
  makeHeader
  [whamlet|
    <p>
      <a href=@{StartR}>Restart game.
  |]
  makeTicTacToeField True f
  makeFooter

postGameR :: TicTacToe -> Handler Html
postGameR f = do
  (px, py) <- runInputPost positionForm -- get info about move and field
  let f' = case posInPicture f (px, py) of
             Just pos -> userMove f pos
             Nothing  -> f

  let f'' = if ended f'
              then f'
              else computerMove f'

  if ended f''
    then redirect (OverR f'')
    else redirect (GameR f'')

positionForm :: FormInput Handler (Double, Double)
positionForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

