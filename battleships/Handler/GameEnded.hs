----------------------------------------------------------------------------
-- |
-- Module      :  Handler.GameEnded
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for an end screen revealing both boards and informing the user
-- whether he has lost or won.

{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

getGameEndedR :: GameStateExt -> Handler Html
getGameEndedR gameE = withGame gameE $ \game -> do
  let humanWon = allSunk (playerFleet $ otherPlayer game)
  let drawn    = isDrawn game
  defaultLayout $ do
    setNormalTitle
    $(widgetFile "board")
    $(widgetFile "gameended")