{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.About
  ( getAboutR
  ) where

import Import
import Handler.Util

getAboutR :: Handler Html
getAboutR = plainLayout $ do 
	setNormalTitle
	$(widgetFile "about")

