{-# LANGUAGE RecordWildCards #-}
module Handler.Util where

import Import
import Diagrams.Prelude
import Data.Maybe
import Logic.Game
import Logic.GameExt
import Logic.CleverAI
import Logic.Render

withGame :: GameStateExt -> (GameState CleverAI -> Handler a) -> Handler a
withGame gameE act = impGame gameE >>= \g -> case g of
  Nothing   -> redirect HomeR
  Just game -> act game

fieldPos :: GameState a -> (Double, Double) -> Maybe Pos
fieldPos (GameState {..}) p = fieldPos' (renderReferenceGrid $ rulesSize gameRules) p

fieldPos' :: BattleDia -> (Double, Double) -> Maybe Pos
fieldPos' dia (px, py)
  = listToMaybe
  $ sample dia
  $ p2 (px, py)

-- | Translates a message of the current application to the current target language.
translateMessage :: MonadHandler m => AppMessage -> m Text
translateMessage msg = do
  langs <- languages
  return $ renderMessage (undefined :: App) langs msg

-- | Set a default html title.
setNormalTitle :: Widget 
setNormalTitle = setTitleI MsgGameName
