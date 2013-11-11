module Handler.Helper
  ( makeTicTacToeField
  , makeHeader
  , makeFooter
  ) where

import Import


makeHeader = do
  [whamlet|
    <h1>
      TicTacToe - α-β-Pruning Edition
  |]

makeTicTacToeField interactive f =
  if interactive
    then do
      [whamlet|
        <p>
          <embed src="@{FieldR f}" type="image/svg+xml" onload="t3init(this);" />
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
    else
      [whamlet|
        <p>
          <embed src="@{FieldR f}" type="image/svg+xml" />
      |]

makeFooter = do
  [whamlet|
    
  |]
