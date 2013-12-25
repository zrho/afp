{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

getGameEndedR :: GameStateExt -> Handler Html
getGameEndedR gameE = withGame gameE $ \game -> do
  let outcome = case allSunk (enemyFleet game) (playerTrack game) of
                  True  -> Won game
                  False -> Lost game
  defaultLayout $ do
    setNormalTitle
    $(widgetFile "gameended")

hasWon :: Turn a -> Bool
hasWon (Won _) = True
hasWon _       = False