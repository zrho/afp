{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home
  ( getHomeR
  ) where

import Import
import Logic.Game
import Handler.Util

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
	setNormalTitle
	$(widgetFile "home")
  where rules = defaultRules