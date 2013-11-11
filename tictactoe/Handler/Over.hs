{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Over (getOverR) where

import Import
import Handler.Helper
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))
import GameLogic.TicTacToe
import GameLogic.Interaction

getOverR :: TicTacToe -> Handler Html
getOverR f = defaultLayout $ do
  makeHeader
  [whamlet|
    <p>
      $if won f
        Computer won.
      $elseif draw f
        It's a draw.
    <p>
      <a href=@{StartR}>Restart game.
  |]
  makeTicTacToeField False f
  [whamlet|
    <embed src="@{SmileyR}" type="image/svg+xml" />
  |]
  makeFooter

