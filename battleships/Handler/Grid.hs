{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Grid
  ( getPlayerGridR
  , getEnemyGridR
  , getLegendR
  , getPlaceGridR
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
  $ diagramContent $ renderPlayerGrid (rulesSize gameRules) (playerFleet currentPlayer) (playerShots otherPlayer) requiredAction gameRules

-- | renders the otherPlayer's grid
getEnemyGridR :: GameStateExt -> Handler TypedContent
getEnemyGridR gameE = withGame gameE $ \(GameState {..}) -> return
  $ diagramContent $ renderEnemyGrid (rulesSize gameRules) (playerShots currentPlayer)

getLegendR :: LegendIcon -> Handler TypedContent
getLegendR = return . diagramContent . renderLegend

getPlaceGridR :: GameStateExt -> Handler TypedContent
getPlaceGridR gameE = withGame gameE $ \(GameState {..}) -> return
  $ diagramContent $ renderWaterGrid (rulesSize gameRules)

legendWidget :: Widget
legendWidget = $(widgetFile "legend")

diagramContent :: (Semigroup m, Monoid m) => QDiagram SVG R2 m -> TypedContent
diagramContent
  = TypedContent typeSvg
  . toContent
  . renderSvg
#if MIN_VERSION_diagrams_svg(0,8,0)
  . renderDia SVG (SVGOptions Absolute Nothing)
#else
  . renderDia SVG (SVGOptions Absolute)
#endif