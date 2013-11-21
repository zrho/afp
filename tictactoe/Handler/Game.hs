{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Field

-------------------------------------------------------------------------------
-- * GET and POST handler

getGameR :: TicTacToe -> Handler Html
getGameR f = do
  (formWidget, enctype) <- generateFormPost fieldNumberForm
  defaultLayout $ do
    [whamlet|
      <p>
        <a href=@{StartR}>Restart game.
    |]
    makeTicTacToeField True f
    [whamlet|
      <noscript>
        <form method=post action=@{GameR f} enctype=#{enctype}>
          ^{formWidget}
          <button>Submit
    |]

postGameR :: TicTacToe -> Handler Html
postGameR f = do
  ((result, _), _) <- runFormPost fieldNumberForm
  maybePos <- case result of
    FormSuccess p -> return $ Just ((p - 1) `mod` 3, (p - 1) `div` 3)
    _ -> do
      (px, py) <- runInputPost positionForm -- get info about move and field
      return $ posInPicture f (px, py)

  let f' = case maybePos of
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

fieldNumberForm :: Html -> MForm Handler (FormResult Int, Widget)
fieldNumberForm = renderDivs $ fieldNumberAForm

fieldNumberAForm :: AForm Handler Int
fieldNumberAForm = areq (checkBool validate errorMsg intField) "Field number (1 to 9): " Nothing where
    errorMsg :: Text
    errorMsg = "Number must be in range from 1 to 9!"
    validate n = n >= 1 && n <= 9

