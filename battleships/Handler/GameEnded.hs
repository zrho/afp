----------------------------------------------------------------------------
-- |
-- Module      :  Handler.GameEnded
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for an end screen revealing both boards and informing the user
-- whether he has lost or won.

module Handler.GameEnded (getGameEndedR, gameEndedView) where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

getGameEndedR :: GameStateExt -> Handler Html
getGameEndedR gameE = withGame gameE $ \game -> gameEndedView game gameE

gameEndedView :: GameState a -> GameStateExt -> Handler Html
gameEndedView game gameE = do
  let humanWon = allSunk (playerFleet $ otherPlayer game)
  let drawn    = isDrawn game
  defaultLayout $ do
    setNormalTitle
    $(widgetFile "board")
    $(widgetFile "gameended")
