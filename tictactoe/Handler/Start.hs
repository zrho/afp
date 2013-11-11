{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Start (getStartR) where

import Import
import GameLogic.TicTacToe
import GameLogic.Search
import Data.Maybe (fromJust)

getStartR :: Handler Html
getStartR = defaultLayout $ do
  [whamlet|
    <h1>
      TicTacToe - α-β-Pruning Edition
    <p>
      <embed src="@{FieldR initialField}" type="image/svg+xml" onload="t3init(this);" />

  |]
  toWidgetBody [julius|

    function t3init(svg) {
      svg.getSVGDocument().onclick = t3click;
    }

    function t3click(event) {
      var form = document.createElement('form');
      form.setAttribute('method','post');
      form.setAttribute('action','@{GameR initialField}');
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
  |] where
    computerStartField = fromJust . nextDraw $ initialField


nextDraw :: TicTacToe -> Maybe TicTacToe
nextDraw
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves
