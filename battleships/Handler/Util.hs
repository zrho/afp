module Handler.Util where

import Import
import Diagrams.Prelude
import Data.Maybe
import Logic.Game
import Logic.GameExt
import Logic.CleverAI
import Logic.Render
import Yesod.Routes.Class
import Data.Serialize (Serialize)

withGame :: GameStateExt -> (GameState CleverAI -> Handler a) -> Handler a
withGame gameE act = impGameH gameE >>= \g -> case g of
  Nothing   -> redirect HomeR
  Just game -> act game

fieldPos :: (Double, Double) -> Maybe Pos
fieldPos p = listToMaybe $ sample renderReferenceGrid $ p2 p

-- | Translates a message of the current application to the current target language.
translateMessage :: MonadHandler m => AppMessage -> m Text
translateMessage msg = do
  langs <- languages
  return $ renderMessage (undefined :: App) langs msg

-- | Set a default html title.
setNormalTitle :: Widget 
setNormalTitle = setTitleI MsgGameName

-- | Returns the static route pointing to the specified legend icon
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

gridStatic :: Route App
gridStatic = StaticR img_grid_svg

impGameH :: Serialize a => GameStateExt -> Handler (Maybe (GameState a))
impGameH game = do
  key <- appKey <$> getYesod
  return $ impGame key game

expGameH :: Serialize a => GameState a -> Handler GameStateExt
expGameH game = do
  key <- appKey <$> getYesod
  expGame key game
