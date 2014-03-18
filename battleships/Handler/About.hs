----------------------------------------------------------------------------
-- |
-- Module      :  Handler.About
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for a static page displaying information about the project.

module Handler.About
  ( getAboutR
  ) where

import Import
import Handler.Util

getAboutR :: Handler Html
getAboutR = do 
  extra <- getExtra
  plainLayout $ do 
    setNormalTitle
    let 
      sourceURL = extraSourceURL extra
      maxTurns = (extraMaxTurns extra) `div` 2
      countdownTurns = extraCountdownTurns extra
    $(widgetFile "about")
