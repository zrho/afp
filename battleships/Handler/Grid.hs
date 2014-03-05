{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Grid
  ( getPlayerGridR
  , getEnemyGridR
  , legendWidget
  ) where

import           Import
import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Text.Blaze.Svg.Renderer.Text (renderSvg)

import           Logic.Render
import           Logic.Game
import           Logic.GameExt
import           Handler.Util

-- | renders the currentPlayer's grid
getPlayerGridR :: GameStateExt -> Action-> Handler TypedContent
getPlayerGridR gameE requiredAction = withGame gameE $ \(GameState {..}) -> return
  $ diagramContent $ renderPlayerGrid (playerFleet currentPlayer) (playerShots otherPlayer) requiredAction

-- | renders the otherPlayer's grid
getEnemyGridR :: GameStateExt -> Handler TypedContent
getEnemyGridR gameE = withGame gameE $ \(GameState {..}) -> return
  $ diagramContent $ renderEnemyGrid (playerFleet otherPlayer) (playerShots currentPlayer) gameRules

legendWidget :: Orientation -> Bool -> Widget
legendWidget orientation movesAllowed = $(widgetFile "legend")

diagramContent :: (Semigroup m, Monoid m) => QDiagram SVG R2 m -> TypedContent
diagramContent
  = TypedContent typeSvg
  . toContent
  . renderSvg
  . renderDiaSVG
