{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Over (getOverR) where

import Import
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))

getOverR :: Handler Html
getOverR = defaultLayout $ do
  [whamlet|
    <embed src="@{SmileyR}" type="image/svg+xml" />
    <a href="@{GameR}" id=newgame>Start new Game
  |]

