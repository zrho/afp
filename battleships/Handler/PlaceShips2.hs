{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips2
  ( getPlaceShips2R
  , postPlaceShips2R
  ) where

import Import
import Data.Aeson (decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Logic.Game
import Logic.GameExt
import Handler.Util
import Debug.Trace

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

getPlaceShips2R :: GameStateExt -> Handler Html
getPlaceShips2R gameE = withGame gameE $ \game@(GameState {..}) -> do
  defaultLayout $ do
    setNormalTitle
    addScript $ StaticR js_jquery_js
    addScript $ StaticR js_json2_js
    addScript $ StaticR js_map_js
    $(widgetFile "placeships2")

postPlaceShips2R :: GameStateExt -> Handler Html
postPlaceShips2R gameE = withGame gameE $ \game@(GameState {..}) -> do
  jsonStr <- runInputPost $ ireq textField "fleetData"
  let ships = decode (BL.fromChunks $ [TE.encodeUtf8 jsonStr]) :: (Maybe [ShipShape])
  traceShow ships (return ())
  case ships of
    Nothing -> redirect $ PlaceShips2R gameE
    Just fleetPlacement -> do
      let 
        fleet = generateFleet fleetPlacement
      g <- expGame game { currentPlayer = currentPlayer { playerFleet = fleet } }
      redirect $ PlayR g
  

  