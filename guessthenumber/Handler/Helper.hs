{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Helper where

import Import

beginForm :: FormInput Handler (Int, Int)
beginForm = (,)
  <$> ireq intField "lower"
  <*> ireq intField "upper"

playForm :: Html -> MForm Handler (FormResult Int, Widget)
playForm = renderDivs $ areq intField "Your guess" Nothing
