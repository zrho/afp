{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Helper where

import Import
import Data.Text
import Logic.GameState

beginForm = (,)
  <$> ireq intField "lower"
  <*> ireq intField "upper"

playForm = renderDivs $ areq intField "Your guess" Nothing


play :: Maybe Int -> GameState -> Handler Html
play (Just n) g | gameAnswer g == n = defaultLayout $(widgetFile "win")
play wrong g = do
  gameExt     <- expGame g
  (form, enc) <- generateFormPost playForm
  let (ub, lb) = gameRange g
  let answer   = gameAnswer g
  defaultLayout $(widgetFile "play")
  
