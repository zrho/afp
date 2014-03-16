----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for a static home page.

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
