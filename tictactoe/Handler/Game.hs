{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Field

-------------------------------------------------------------------------------
-- * GET and POST handler (both of which use the function gameHandler)

getGameR :: TicTacToe -> Handler Html
getGameR f = gameHandler f

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
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- * gameHandler prints the given field

gameHandler :: TicTacToe -> Handler Html
gameHandler f = defaultLayout $ do
  [whamlet|
    <h1>
      TicTacToe - α-β-Pruning Edition
    <p>
      <a href=@{StartR}>Restart game.
    <p>
      <embed src="@{FieldR f}" type="image/svg+xml" onload="t3init(this);" />

    $if isJust (winner f) || null (freePositions f)
      <p>
        $if isJust (winner f)
          You lost!
        $if null (freePositions f)
          Draw!
      <p>
        <embed src="@{SmileyR}" type="image/svg+xml" />
      <p>
        <a href=@{StartR}>Start new Game

  |]
  toWidgetBody [julius|

    function t3init(svg) {
      svg.getSVGDocument().onclick = t3click;
    }

    function t3click(event) {
      var form = document.createElement('form');
      form.setAttribute('method','post');
      form.setAttribute('action','@{GameR f}');
      var hiddenField = document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','X');
      hiddenField.setAttribute('value',event.clientX);
      form.appendChild(hiddenField);
      var hiddenField=document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','Y');
      hiddenField.setAttribute('value',-event.clientY);
      form.appendChild(hiddenField);
      document.body.appendChild(form);
      form.submit();
    }
  |]

