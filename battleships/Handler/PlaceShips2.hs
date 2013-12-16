{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips2
  ( getPlaceShips2R
  , postPlaceShips2R
  ) where

import Import
import Data.Aeson (decode)
import qualified Data.Text  as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Logic.Game
import Logic.GameExt
import Logic.Render
import Logic.StupidAI
import Handler.Util
import Data.List as L
import Data.Serialize (Serialize)
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
  let ships = decode (BL.fromChunks $ [TE.encodeUtf8 jsonStr]) :: (Maybe [Ship])
  traceShow ships (return ())
  case ships of
    Nothing -> redirect $ PlaceShips2R gameE
    Just fleet -> do
      g <- expGame game { playerFleet = fleet }
      redirect $ PlayR g
  

  