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
import Logic.AIUtil
import Logic.CleverAI
import Handler.Util

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

getPlaceShipsR :: Rules -> Handler Html
getPlaceShipsR gameRules = defaultLayout $ do
  setNormalTitle
  messageRender <- getMessageRender
  addScript $ StaticR js_jquery_js
  addScript $ StaticR js_json2_js
  addScript $ StaticR js_map_js
  $(widgetFile "board")
  $(widgetFile "placeships2")

postPlaceShipsR :: Rules -> Handler Html
postPlaceShipsR rules = do
  ships <- getPostedFleet
  case ships of
    Nothing             -> redirect $ PlaceShipsR rules
    Just fleetPlacement -> startGame rules fleetPlacement

startGame :: Rules -> FleetPlacement -> Handler Html
startGame rules fleetPlacement = do
  game  <- liftIO $ (newGame rules fleetPlacement HumanPlayer :: IO (GameState CleverAI))
  gameE <- expGameH game
  redirect $ PlayR gameE

postPlaceShipsRndR :: Rules -> Handler TypedContent
postPlaceShipsRndR rules = do
  fleet  <- fmap (fromMaybe []) getPostedFleet
  fleet' <- liftIO $ initShips rules fleet
  return $ jsonFleet $ fromMaybe [] $ fleet'
  
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