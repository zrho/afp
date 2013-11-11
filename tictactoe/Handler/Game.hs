{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Search
import Handler.Field

-------------------------------------------------------------------------------
-- * GET and POST handler (both of which use the function gameHandler)

getGameR :: TicTacToe -> Handler Html
getGameR f = gameHandler f -- start with empty field

postGameR :: TicTacToe -> Handler Html
postGameR f = do
  (px, py) <- runInputPost positionForm -- get info about move and field
  let
    newField =
      case posInPicture f (px, py) of
        Just pos -> play f pos
        Nothing  -> f
  redirect (GameR newField) where

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
--------------------------------------------------------------------------------

-------------------------------------------------------------------------
-- * Game Logic

play :: TicTacToe -> Pos -> TicTacToe
play field pos = case getField field pos of
  Just _  -> field
  Nothing -> maybe field' id $ nextDraw field' where
    field' = setField field X pos

nextDraw :: TicTacToe -> Maybe TicTacToe
nextDraw
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves
