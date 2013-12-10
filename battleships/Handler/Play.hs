{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Play (getPlayR, postPlayR) where

import Import
import qualified Data.Text  as T
import Logic.GameFramework
import Data.Maybe

import Handler.PlayHelper

fireShotForm :: Html -> MForm Handler (FormResult (Maybe Pos), Widget)
fireShotForm extra = do
  (posXRes, posXView) <- mreq doubleField posXFieldSettings Nothing
  (posYRes, posYView) <- mreq doubleField posYFieldSettings Nothing
  grid <- renderFireShotGrid
  let posRes = coordinatesToPos <$> posXRes <*> posYRes <*> pure grid
  let widget = do
                 toWidget
                   [julius|
                      function playInit(svg) {
                        svg.getSVGDocument().onclick = fireShotClick;
                      }

                      function fireShotClick(event) {
                        var form = document.getElementById('fireShotForm');
                        var hiddenField = document.getElementById('posX');
                        hiddenField.setAttribute('value',event.clientX);
                        var hiddenField = document.getElementById('posY');
                        hiddenField.setAttribute('value',-event.clientY);
                        form.submit();
                      }
                   |]
                 [whamlet|
                     #{extra}
                       <p>
                         Click on a cell to fire a shot.
                       <div style="visibility:hidden">
                         ^{fvInput posXView}
                         ^{fvInput posYView}
                         <input type='submit'>
                       <p>
                         <embed src="@{PlayerGridR}" type="image/svg+xml" onload="playInit(this);" />
                 |]
  return (posRes
         , widget) where
    posXFieldSettings = FieldSettings undefined Nothing (Just "posX") Nothing []
    posYFieldSettings = FieldSettings undefined Nothing (Just "posY") Nothing []

getPlayR :: Handler Html
getPlayR = do
  (formWidget, enctype) <- generateFormPost fireShotForm
  -- actual output:
  defaultLayout $ do
    setTitle "Fire a shot! – Battleships"
    $(widgetFile "play")

postPlayR :: Handler Html
postPlayR = do
  ((formResult, _), _) <- runFormPost fireShotForm
  (fleet, grid) <- readSession
  case formResult of
    FormSuccess hitPos -> writeSession (handleShot fleet grid (fromJust hitPos))

    FormFailure text   -> setMessage . toHtml . T.concat $ text
    _                  -> setMessage "Form missing."
  redirect PlayR