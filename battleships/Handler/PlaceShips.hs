{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips where

import Import
import qualified Data.Text  as T
import Logic.Game
import Logic.GameExt
import Logic.Render
import Logic.StupidAI
import Handler.Util
import Data.Maybe
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
      defaultLayout $ do
        setTitle "Place your ships! – Battleships"
        $(widgetFile "placeships")

postPlaceShipsR :: GameStateExt -> Handler Html
postPlaceShipsR gameE = withGame gameE $ \game@(GameState {..}) -> do
  let delta = shipsDelta (rulesShips gameRules) playerFleet
  ((formResult, _), _) <- runFormPost $ placeShipsForm delta game
  case formResult of
    FormSuccess Nothing  -> setMessage "Please click in a cell."
    FormSuccess (Just s)
      | shipAdmissible gameRules playerFleet s -> do
        let game' = game { playerFleet = s : playerFleet } :: GameState StupidAI
        expGame game' >>= redirect . PlaceShipsR
      | otherwise        -> setMessage "Ship cannot be placed there."
    FormFailure text     -> setMessage . toHtml . T.intercalate " " $ text
    FormMissing          -> setMessage "Form missing."
  redirect $ PlaceShipsR gameE

shipAdmissible :: Rules -> Fleet -> Ship -> Bool
shipAdmissible (Rules {..}) fleet ship = rangeCheck && freeCheck where
  rangeCheck     = L.all inRange shipCoords
  freeCheck      = L.all (isNothing . shipAt fleet) shipCoords
  inRange (x, y) = L.and [ 0 <= x, x < w, 0 <= y, y < h]
  (w, h)         = rulesSize
  shipCoords     = shipCoordinates ship

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

groupToOption :: [Int] -> (Text, Int)
groupToOption is = (T.pack text, L.head is) where
  text = show (L.head is) L.++ " (" L.++ show (L.length is) L.++ " times)"

orientationList :: [(Text, Orientation)]
orientationList =
  [ ("Horizontal", Horizontal)
  , ("Vertical", Vertical)
  ]


{-



-}