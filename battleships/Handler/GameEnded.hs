{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

getGameEndedR :: GameStateExt -> Handler Html
getGameEndedR gameE = withGame gameE $ \game -> do
  let humanWon = allSunk (playerFleet $ otherPlayer $ game)
  defaultLayout $ do
    setNormalTitle
    $(widgetFile "board")
    $(widgetFile "gameended")