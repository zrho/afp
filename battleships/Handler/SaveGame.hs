----------------------------------------------------------------------------
-- |
-- Module      :  Handler.SaveGame
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for a page displaying the link that leads to the current game state.

module Handler.SaveGame
  ( getSaveGameR
  ) where

import Import
import Handler.Util
import Logic.GameExt

getSaveGameR :: GameStateExt -> Handler Html
getSaveGameR gameE = withGame gameE $ \game -> plainLayout $ do
    setNormalTitle
    $(widgetFile "savegame")