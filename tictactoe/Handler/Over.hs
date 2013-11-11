{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Over (getOverR) where

import Import
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))
import GameLogic.TicTacToe

getOverR :: TicTacToe -> Handler Html
getOverR _ = defaultLayout $ do
  [whamlet|
    <embed src="@{SmileyR}" type="image/svg+xml" />
  |]

