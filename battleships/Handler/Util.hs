{-# LANGUAGE RecordWildCards #-}
module Handler.Util where

import Import
import Diagrams.Prelude
import Data.Maybe
import Logic.Game
import Logic.GameExt
import Logic.StupidAI
import Logic.Render

withGame :: GameStateExt -> (GameState StupidAI -> Handler a) -> Handler a
withGame gameE act = impGame gameE >>= \g -> case g of
  Nothing   -> redirect HomeR
  Just game -> act game

fieldPos :: GameState a -> (Double, Double) -> Maybe Pos
fieldPos (GameState {..}) p = fieldPos' (renderEnemyGrid playerTrack) p

fieldPos' :: BattleDia -> (Double, Double) -> Maybe Pos
fieldPos' dia (px, py)
  = listToMaybe
  $ sample dia
  $ p2 (px, py)