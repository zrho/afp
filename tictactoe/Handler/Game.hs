{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Maybe ()
import GameLogic.TicTacToe
import GameLogic.Search
import Handler.Field

-------------------------------------------------------------------------------
-- * GET and POST handler (both of which use the function gameHandler)

getGameR :: Handler Html
getGameR = gameHandler initialField -- start with empty field

postGameR :: Handler Html
postGameR = do
  (px, py, fx, fo) <- runInputPost positionForm -- get info about move and field
  let f = TicTacToe fx fo -- build the field
  case posInPicture f (px, py) of
    Just pos -> gameHandler $ play f pos
    Nothing  -> gameHandler initialField

positionForm :: FormInput Handler (Double, Double, Int, Int)
positionForm = (,,,) <$> ireq doubleField "X" <*> ireq doubleField "Y" <*> ireq intField "fx" <*> ireq intField "fo"
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- * gameHandler prints the given field

gameHandler :: TicTacToe -> Handler Html
gameHandler f@(TicTacToe fx fo) = defaultLayout $ do
  [whamlet|
    <embed src="@{FieldR f}" type="image/svg+xml" onload="t3init(this);" />
  |]
  toWidgetBody [julius|

    $if isJust (winner f) || length (freePositions f) < 1
    window.location='@{OverR}';
    $else

    function t3init(svg) {
      svg.getSVGDocument().onclick = t3click;
    }

    function t3click(event) {
      var form = document.createElement('form');
      form.setAttribute('method','post');
      form.setAttribute('action','@{GameR}');
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
      var hiddenField = document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','fx');
      hiddenField.setAttribute('value',#{toJSON $ show fx});
      form.appendChild(hiddenField);
      var hiddenField = document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','fo');
      hiddenField.setAttribute('value',#{toJSON $ show fo});
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
