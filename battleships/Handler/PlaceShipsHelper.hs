module Handler.PlaceShipsHelper where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.GameFramework
import Logic.Rendering

import Data.Serialize
import Data.List as L
import Data.Maybe as M
import Data.ByteString as BS

type ShipList = [Int] -- ship sizes

defaultRules :: Rules
defaultRules = Rules { mapSize = (10,10) }

defaultShipList :: ShipList
defaultShipList = [5, 4, 4, 3, 3, 3, 2, 2, 2, 2]

writeSession :: MonadHandler m => Rules -> ShipList -> Fleet -> m ()
writeSession rules shipList fleet = do
  setSessionBS "rules" $ encode rules
  setSessionBS "shipList" $ encode shipList
  setSessionBS "fleetPending" $ encode fleet

readSession :: MonadHandler m => m (Rules, ShipList, Fleet)
readSession = do
  -- read rules from session cookie:
  rulesSerialized <- lookupSessionBS "rules"
  let rules = readRules rulesSerialized
  -- read already built fleet from session cookie:
  fleetSerialized <- lookupSessionBS "fleetPending"
  let fleet = readFleet fleetSerialized
  return (rules, defaultShipList, fleet) where
    readRules serialized = case serialized of
      Nothing -> defaultRules
      Just s -> either (const defaultRules) id $ decode s
    readFleet :: Maybe BS.ByteString -> Fleet
    readFleet serialized = case serialized of
      Nothing -> []
      Just s -> either (const []) id $ decode s

renderPlaceShipsGrid :: MonadHandler m => m BattleDia
renderPlaceShipsGrid = do
  (rules, _, fleet) <- readSession
  let (width,height) = mapSize rules
  let g = A.listArray ((0,0),(width - 1, height - 1)) (repeat False)
  return $ renderPlayerGrid fleet g

coordinatesToPos :: Double -> Double -> BattleDia -> Maybe Pos
coordinatesToPos x y grid = M.listToMaybe . sample grid $ p2 (x, y)

