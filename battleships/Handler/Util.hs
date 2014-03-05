----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Util
-- Stability   :  experimental
-- Portability :  portable
--
-- Several utilities for the handlers of this package.

module Handler.Util
  ( withGame
  , fieldPos
  , setNormalTitle
  , legendStatic
  , gridStatic
  , impGameH
  , expGameH
  ) where

import Import
import Diagrams.Prelude
import Data.Maybe
import Logic.Game
import Logic.GameExt
import Logic.CleverAI
import Logic.Render
import Yesod.Routes.Class
import Data.Serialize (Serialize)

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
withGame :: GameStateExt -> (GameState CleverAI -> Handler a) -> Handler a
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