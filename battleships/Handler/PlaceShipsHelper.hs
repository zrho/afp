module Handler.PlaceShipsHelper where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.Game
import Logic.Render

import Data.Serialize
import Data.List as L
import Data.Maybe as M
import Data.ByteString as BS

defaultRules :: Rules
defaultRules = Rules
  { rulesSize  = (10, 10)
  , rulesShips = [5, 4, 4, 3, 3, 3, 2, 2, 2, 2]
  }

writeSession :: MonadHandler m => Rules -> Fleet -> m ()
writeSession rules fleet = do
  setSessionBS "rules" $ encode rules
  setSessionBS "fleetPending" $ encode fleet

readSession :: MonadHandler m => m (Rules, Fleet)
readSession = do
  -- read rules from session cookie:
  rulesSerialized <- lookupSessionBS "rules"
  let rules = readRules rulesSerialized
  -- read already built fleet from session cookie:
  fleetSerialized <- lookupSessionBS "fleetPending"
  let fleet = readFleet fleetSerialized
  return (rules, fleet) where
    readRules serialized = case serialized of
      Nothing -> defaultRules
      Just s -> either (const defaultRules) id $ decode s
    readFleet :: Maybe BS.ByteString -> Fleet
    readFleet serialized = case serialized of
      Nothing -> []
      Just s -> either (const []) id $ decode s

renderPlaceShipsGrid :: MonadHandler m => m BattleDia
renderPlaceShipsGrid = do
  (rules, fleet) <- readSession
  let (width,height) = rulesSize rules
  let g = A.listArray ((0,0),(width - 1, height - 1)) (repeat False)
  return $ renderPlayerGrid fleet g

coordinatesToPos :: Double -> Double -> BattleDia -> Maybe Pos
coordinatesToPos x y grid = M.listToMaybe . sample grid $ p2 (x, y)

