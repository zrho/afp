{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips
( getPlaceShipsR
, postPlaceShipsR
) where

import Import
import qualified Data.Text  as T
import Logic.Game
import Logic.GameExt
import Logic.Render
import Logic.CleverAI
import Handler.Util
import Data.List as L
import Data.Serialize (Serialize)

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

getPlaceShipsR :: GameStateExt -> Handler Html
getPlaceShipsR gameE = withGame gameE $ \game@(GameState {..}) -> do
  let delta = shipsDelta (rulesShips gameRules) playerFleet
  case delta of
    [] -> redirect $ PlayR gameE
    _  -> do
      let form = placeShipsForm delta game
      (formWidget, enctype) <- generateFormPost form
      newGameE <- exportNewGame gameRules -- needed for resetting
      defaultLayout $ do
        setNormalTitle
        $(widgetFile "placeships")

exportNewGame :: MonadHandler m => Rules -> m GameStateExt
exportNewGame rules
 = expGame =<< (liftIO $ (newGame rules [] :: IO (GameState CleverAI)))

postPlaceShipsR :: GameStateExt -> Handler Html
postPlaceShipsR gameE = withGame gameE $ \game@(GameState {..}) -> do
  let delta = shipsDelta (rulesShips gameRules) playerFleet
  ((formResult, _), _) <- runFormPost $ placeShipsForm delta game
  case formResult of
    FormSuccess Nothing  -> translateMessage MsgNoCell >>= setMessage . toHtml
    FormSuccess (Just s)
      | shipAdmissible gameRules playerFleet s -> do
        let game' = game { playerFleet = s : playerFleet } :: GameState CleverAI
        expGame game' >>= redirect . PlaceShipsR
      | otherwise        -> translateMessage MsgInvalidPlacement >>= setMessage . toHtml
    FormFailure text     -> setMessage . toHtml . T.intercalate " " $ text
    FormMissing          -> translateMessage MsgNoForm >>= setMessage . toHtml
  redirect $ PlaceShipsR gameE

shipsDelta :: ShipList -> Fleet -> ShipList
shipsDelta shipList fleet = shipList \\ fmap shipSize fleet

-------------------------------------------------------------------------------
-- * Form
-------------------------------------------------------------------------------

type ShipList = [Int]

placeShipsForm
  :: Serialize a
  => ShipList   -- ^ ships to be placed yet
  -> GameState a
  -> Html
  -> MForm Handler (FormResult (Maybe Ship), Widget)

placeShipsForm ships g@(GameState {..}) extra = do
  (sizeRes, sizeView) <- mreq (selectFieldList ships') "Size" Nothing
  (posXRes, posXView) <- mreq doubleField (posSettings posXId) Nothing
  (posYRes, posYView) <- mreq doubleField (posSettings posYId) Nothing
  (orRes, orView)     <- mreq (selectFieldList orientationList) "Orientation" Nothing
  gameE               <- expGame g
  let shipRes = buildShip <$> posXRes <*> posYRes <*> sizeRes <*> orRes
  let widget  = $(widgetFile "placeshipsform")
  return (shipRes, widget) where
    posSettings n = FieldSettings undefined Nothing (Just n) Nothing []
    posXId = "posX" :: Text
    posYId = "posY" :: Text
    grid   = renderPlaceGrid playerFleet (rulesSize gameRules)
    ships' = L.map groupToOption $ L.group ships
    buildShip x y sz ori = fmap (\p -> Ship p sz ori) $ fieldPos' grid (x, y)

groupToOption :: [Int] -> (AppMessage, Int)
groupToOption is = (text, size) where
  text = MsgShipOption size (L.length is)
  size = L.head is

orientationList :: [(AppMessage, Orientation)]
orientationList =
  [ (MsgHorizontal, Horizontal)
  , (MsgVertical, Vertical)
  ]
