{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlaceShips (getPlaceShipsR, postPlaceShipsR) where

import Import
import qualified Data.Text  as T
import Data.Serialize
import Logic.GameFramework
import Data.List as L
import Data.ByteString as BS
import Data.Maybe

import Handler.PlaceShipsHelper
import Logic.Session

data ShipData = ShipData {
  selectedSize :: Int
, selectedPos :: Maybe Pos
, selectedOrientation :: Orientation
} deriving Show

placeShipsForm :: ShipSizeList -> Html -> MForm Handler (FormResult ShipData, Widget)
placeShipsForm toBePlacedYet extra = do
  (sizeRes, sizeView) <- mreq (selectFieldList sizeList) "Size" Nothing
  (posXRes, posXView) <- mreq doubleField posXFieldSettings Nothing
  (posYRes, posYView) <- mreq doubleField posYFieldSettings Nothing
  (orientationRes, orientationView) <- mreq (selectFieldList orientationList) "Orientation" Nothing
  grid <- renderPlaceShipsGrid
  let posRes = coordinatesToPos <$> posXRes <*> posYRes <*> pure grid
  let shipDataRes = ShipData <$> sizeRes <*> posRes <*> orientationRes
  let widget = $(widgetFile "placeshipsform")
  return (shipDataRes, widget) where
    sizeList :: [(Text, Int)]
    sizeList = L.map groupToOption . L.group $ toBePlacedYet where
      groupToOption :: [Int] -> (Text, Int)
      groupToOption is = (T.pack text, L.head is) where
        text = show (L.head is) L.++ " (" L.++ show (L.length is) L.++ " times)"
    orientationList :: [(Text, Orientation)]
    orientationList = [("Horizontal" :: Text, Horizontal), ("Vertical", Vertical)]
    posXId :: Text
    posXId = "posX"
    posYId :: Text
    posYId = "posY"
    posXFieldSettings = FieldSettings undefined Nothing (Just posXId) Nothing []
    posYFieldSettings = FieldSettings undefined Nothing (Just posYId) Nothing []

getPlaceShipsR :: Handler Html
getPlaceShipsR = do
  rules <- readFromSessionDefault rulesKey standardRules
  shipList <- readFromSessionDefault shipListKey standardShipSizes
  fleet <- readFromSessionDefault fleetPendingKey []

  let toBePlacedYet = shipsToBePlacedYet shipList fleet
  if L.null toBePlacedYet then redirect FinishedPlacingR else do
  (formWidget, enctype) <- generateFormPost (placeShipsForm toBePlacedYet)
  defaultLayout $ do
    setTitle "Place your ships! – Battleships"
    $(widgetFile "placeships")

postPlaceShipsR :: Handler Html
postPlaceShipsR = do
  rules <- readFromSessionDefault rulesKey standardRules
  shipList <- readFromSessionDefault shipListKey standardShipSizes
  fleet <- readFromSessionDefault fleetPendingKey []

  let toBePlacedYet = shipsToBePlacedYet shipList fleet
  ((formResult, _), _) <- runFormPost (placeShipsForm toBePlacedYet)
  case formResult of
    FormSuccess shipData -> do
      let maybePos = selectedPos shipData
      case maybePos of
        Nothing -> setMessage "Please click in a cell."
        Just pos -> let newShip = Ship { shipSize = selectedSize shipData
                                       , shipPosition = pos
                                       , shipOrientation = selectedOrientation shipData
                                       } in
          if canShipBePlaced rules fleet newShip
          then do
                 let newFleet = fleet ++ [newShip]
                 writeToSession fleetPendingKey newFleet
          else setMessage "Ship cannot be placed there."
    FormFailure text     -> setMessage . toHtml . T.intercalate " " $ text
    FormMissing          -> setMessage "Form missing."
  redirect PlaceShipsR

