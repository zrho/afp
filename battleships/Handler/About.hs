----------------------------------------------------------------------------
-- |
-- Module      :  Handler.About
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for a static page displaying information about the project.

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

