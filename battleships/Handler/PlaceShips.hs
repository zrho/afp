{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlaceShips (getPlaceShipsR, postPlaceShipsR) where

import Import
import qualified Data.Text  as T
import Logic.Game
import Data.Maybe
import Data.List as L

import Handler.PlaceShipsHelper

data ShipData = ShipData {
  selectedSize :: Int
, selectedPos :: Maybe Pos
, selectedOrientation :: Orientation
} deriving Show

placeShipsForm :: ShipList -> Html -> MForm Handler (FormResult ShipData, Widget)
placeShipsForm toBePlacedYet extra = do
  (sizeRes, sizeView) <- mreq (selectFieldList sizeList) "Size" Nothing
  (posXRes, posXView) <- mreq doubleField posXFieldSettings Nothing
  (posYRes, posYView) <- mreq doubleField posYFieldSettings Nothing
  (orientationRes, orientationView) <- mreq (selectFieldList orientationList) "Orientation" Nothing
  grid <- renderPlaceShipsGrid
  let posRes = coordinatesToPos <$> posXRes <*> posYRes <*> pure grid
  let shipDataRes = ShipData <$> sizeRes <*> posRes <*> orientationRes
  let widget = do
                 toWidget
                   [julius|
                      function placeShipsInit(svg) {
                        svg.getSVGDocument().onclick = placeShipsClick;
                      }

                      function placeShipsClick(event) {
                        var form = document.getElementById('placeShipsForm');
                        var hiddenField = document.getElementById('posX');
                        hiddenField.setAttribute('value',event.clientX);
                        var hiddenField = document.getElementById('posY');
                        hiddenField.setAttribute('value',-event.clientY);
                        form.submit();
                      }
                   |]
                 [whamlet|
                     #{extra}
                       <p>
                         Insert a ship of size #
                         ^{fvInput sizeView} with orientation #
                         ^{fvInput orientationView} at position: (click in the field)
                       <div style="visibility:hidden">
                         ^{fvInput posXView}
                         ^{fvInput posYView}
                         <input type='submit'>
                       <p>
                         <embed src="@{PlaceShipsGridR}" type="image/svg+xml" onload="placeShipsInit(this);" />
                 |]
  return (shipDataRes
         , widget) where
    sizeList :: [(Text, Int)]
    sizeList = fmap (\i -> (T.pack $ show i, i)) . nub $ toBePlacedYet
    orientationList :: [(Text, Orientation)]
    orientationList = [("Horizontal" :: Text, Horizontal), ("Vertical", Vertical)]
    posXFieldSettings = FieldSettings undefined Nothing (Just "posX") Nothing []
    posYFieldSettings = FieldSettings undefined Nothing (Just "posY") Nothing []

getPlaceShipsR :: Handler Html
getPlaceShipsR = do
  (rules, fleet) <- readSession
  let shipList = rulesShips rules
  (formWidget, enctype) <- generateFormPost (placeShipsForm shipList)
  let toBePlacedYet = shipsToBePlacedYet shipList fleet
  -- actual output:
  defaultLayout $ do
    setTitle "Place your ships! – Battleships"
    $(widgetFile "placeships")

postPlaceShipsR :: Handler Html
postPlaceShipsR = do
  (rules, fleet) <- readSession
  let shipList = rulesShips rules
  ((formResult, _), _) <- runFormPost (placeShipsForm shipList)
  case formResult of
    FormSuccess shipData -> do
      let maybePos = selectedPos shipData
      case maybePos of
        Nothing -> setMessage "Please click in a cell."
        Just pos -> let newShip = Ship { shipSize = selectedSize shipData
                                       , shipPosition = pos
                                       , shipOrientation = selectedOrientation shipData
                                       } in
          if canBePlaced rules fleet newShip
          then do
                 let newFleet = fleet ++ [newShip]
                 writeSession rules newFleet
          else setMessage "Ship cannot be placed there."
    FormFailure text     -> setMessage . toHtml . T.concat $ text
    _                    -> setMessage "Form missing."
  redirect PlaceShipsR

canBePlaced :: Rules -> Fleet -> Ship -> Bool
canBePlaced rules fleet ship
 = L.and [ L.all (inRange rules) shipCoord
       , L.all (isNothing . shipAt fleet) shipCoord
       ]
   where
     shipCoord = shipCoordinates ship
     inRange (Rules { rulesSize = (w, h) }) (x, y)
       = L.and [ 0 <= x, x < w, 0 <= y, y < h]

shipsToBePlacedYet :: ShipList -> Fleet -> ShipList
shipsToBePlacedYet shipList fleet
 = shipList L.\\ L.map shipSize fleet
