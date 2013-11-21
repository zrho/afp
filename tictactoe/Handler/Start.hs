{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Start (getStartR) where

import Import
import Handler.Field
import GameLogic.TicTacToe
import GameLogic.Search
import GameLogic.Interaction
import Data.Maybe (fromJust)

getStartR :: Handler Html
getStartR = do
  (formWidget, enctype) <- generateFormPost fieldNumberForm
  defaultLayout $ do
    $(widgetFile "start")
    makeTicTacToeField True initialField
    [whamlet|
      <noscript>
        <form method=post action=@{GameR initialField} enctype=#{enctype}>
          ^{formWidget}
          <button>Submit
    |]
    where
      computerStartField = computerMove initialField

fieldNumberForm :: Html -> MForm Handler (FormResult Int, Widget)
fieldNumberForm = renderDivs $ fieldNumberAForm

fieldNumberAForm :: AForm Handler Int
fieldNumberAForm = areq (checkBool validate errorMsg intField) "Field number (1 to 9): " Nothing where
    errorMsg :: Text
    errorMsg = "Number must be in range from 1 to 9!"
    validate n = n >= 1 && n <= 9
