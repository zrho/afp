----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Util
-- Stability   :  experimental
-- Portability :  portable
--
-- Several utilities for the handlers of this package.
{-# LANGUAGE CPP #-}
module Handler.Util
  ( withGame
  , fieldPos
  , setNormalTitle
  , legendStatic
  , timedLegendStatic
  , gridStatic
  , impGameH
  , expGameH
  , renderDiaSVG
  , renderSvgHtml
  , playerGridHtml
  , enemyGridHtml
  ) where

import Import
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Internal (Svg)
import Data.Maybe
import Logic.Game
import Logic.GameExt
import Logic.Render
import Yesod.Routes.Class
import Data.Serialize (Serialize)
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Data.Text.Lazy (toStrict)
import Data.Text as T

-------------------------------------------------------------------------------
-- * Game Import/Export
-------------------------------------------------------------------------------

-- | Imports a game state in the Handler monad.
impGameH :: Serialize a => GameStateExt -> Handler (Either String (GameState a))
impGameH game = do
  key <- appKey <$> getYesod
  return $ impGame key game

-- | Exports a game state in the Handler monad.
expGameH :: Serialize a => GameState a -> Handler GameStateExt
expGameH game = do
  key <- appKey <$> getYesod
  expGame key game

-- | Bracket for actions in the Handler monad that require the game state.
withGame :: GameStateExt -> (GameState DefaultAI -> Handler a) -> Handler a
withGame gameE act = impGameH gameE >>= \g -> case g of
  Left _     -> redirect HomeR
  Right game -> act game

-------------------------------------------------------------------------------
-- * Static routes
-------------------------------------------------------------------------------

-- | Static route for the specified legend icon.
legendStatic :: LegendIcon -> Route App
legendStatic ico = StaticR $ case ico of
  LIShipWithArrow -> img_LIShipWithArrow_svg
  LIShipMovable   -> img_LIShipMovable_svg
  LIShipImmovable -> img_LIShipImmovable_svg
  LIShipHit       -> img_LIShipHit_svg
  LIShipSunk      -> img_LIShipSunk_svg
  LIFogOfWar      -> img_LIFogOfWar_svg
  LIWater         -> img_LIWater_svg
  LILastShot      -> img_LILastShot_svg

timedLegendStatic :: TimedLegendIcon -> Route App
timedLegendStatic ico = StaticR $ case ico of
  TLIWater 0   -> img_TLIWater_0_svg
  TLIWater 5   -> img_TLIWater_5_svg
  TLIWater 10  -> img_TLIWater_10_svg
  TLIWater 15  -> img_TLIWater_15_svg
  TLIWater 20  -> img_TLIWater_20_svg
  TLIMarker 0  -> img_TLIMarker_0_svg
  TLIMarker 5  -> img_TLIMarker_5_svg
  TLIMarker 10 -> img_TLIMarker_10_svg
  TLIMarker 15 -> img_TLIMarker_15_svg
  TLIMarker 20 -> img_TLIMarker_20_svg
  _            -> img_LIFogOfWar_svg

-- | Static route for the grid.
gridStatic :: Route App
gridStatic = StaticR img_grid_svg

-------------------------------------------------------------------------------
-- * Misc
-------------------------------------------------------------------------------

-- | Converts coordinates in the grid SVG to a field position.
fieldPos :: (Double, Double) -> Maybe Pos
fieldPos p = listToMaybe $ sample renderReferenceGrid $ p2 p

-- | Set a default html title.
setNormalTitle :: Widget 
setNormalTitle = setTitleI MsgGameName

playerGridHtml :: GameState a -> Action -> Html
playerGridHtml (GameState {..}) requiredAction
  = renderSvgHtml
  $ renderPlayerGrid
    (playerFleet currentPlayer)
    (playerShots otherPlayer)
    requiredAction
    gameRules
    turnNumber

enemyGridHtml :: GameState a -> Bool -> Html
enemyGridHtml (GameState {..}) uncoverFleet
  = renderSvgHtml
  $ renderEnemyGrid
    (playerFleet otherPlayer)
    (playerShots currentPlayer)
    gameRules
    noviceModeOpt
    turnNumber
    (uncoverFleet || devModeOpt)

-- | Renders a diagram as an SVG text.
--
-- Uses breakOn to omit doctype and xml declaration, so the text can
-- be embedded in HTML. TODO: Is there a less hacky way to do that?

renderSvgHtml :: (Semigroup m, Monoid m) => QDiagram SVG R2 m -> Html
renderSvgHtml
  = preEscapedToMarkup
  . snd
  . T.breakOn "<svg xmlns="
  . toStrict
  . renderSvg
  . renderDiaSVG

renderDiaSVG :: (Monoid m, Semigroup m) =>
                QDiagram SVG R2 m -> Text.Blaze.Svg.Internal.Svg
renderDiaSVG =
#if MIN_VERSION_diagrams_svg(0,8,0)
  renderDia SVG (SVGOptions Absolute Nothing)
#else
  renderDia SVG (SVGOptions Absolute)
#endif
