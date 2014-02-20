{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips
  ( getPlaceShipsR
  , postPlaceShipsR
  , postPlaceShipsRndR
  ) where

import Import
import Data.Aeson (encode, decode)
import Data.Maybe
--import Data.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Logic.Game
import Logic.GameExt
import Logic.AIUtil
import Handler.Util

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

getPlaceShipsR :: GameStateExt -> Handler Html
getPlaceShipsR gameE = withGame gameE $ \game@(GameState {..}) -> do
  defaultLayout $ do
    setNormalTitle
    addScript $ StaticR js_jquery_js
    addScript $ StaticR js_json2_js
    addScript $ StaticR js_map_js
    fleet <- liftIO $ initShips defaultRules []
    $(widgetFile "board")
    $(widgetFile "placeships2")

postPlaceShipsR :: GameStateExt -> Handler Html
postPlaceShipsR gameE = withGame gameE $ \game@(GameState {..}) -> do
  ships <- getPostedFleet
  case ships of
    Nothing             -> redirect $ PlaceShipsR gameE
    Just fleetPlacement -> do
      let fleet = generateFleet fleetPlacement
      g <- expGame game { currentPlayer = currentPlayer { playerFleet = fleet } }
      redirect $ PlayR g

postPlaceShipsRndR :: GameStateExt -> Handler TypedContent
postPlaceShipsRndR gameE = withGame gameE $ \(GameState {..}) -> do
  fleet  <- fmap (fromMaybe []) getPostedFleet
  fleet' <- liftIO $ initShips gameRules fleet
  return $ jsonFleet fleet'
  
getX :: ShipShape -> Int 
getX s = fst $ shipPosition s

getY :: ShipShape -> Int 
getY s = snd $ shipPosition s

getOrientation :: ShipShape -> Int 
getOrientation s = fromEnum $ shipOrientation s

getPostedFleet :: Handler (Maybe [ShipShape])
getPostedFleet = do
  jsonStr <- runInputPost $ ireq textField "fleetData"
  return $ decode (BL.fromChunks [TE.encodeUtf8 jsonStr])
  
jsonFleet :: FleetPlacement -> TypedContent
jsonFleet
  = TypedContent typeJson
  . toContent
  . encode