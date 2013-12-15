{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.PlaceShips2
  ( getPlaceShips2R
  , postPlaceShips2R
  ) where

import Import
import qualified Data.Text  as T
import Logic.Game
import Logic.GameExt
import Logic.Render
import Logic.StupidAI
import Handler.Util
import Data.List as L
import Data.Serialize (Serialize)

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
  redirect $ PlaceShips2R gameE

  