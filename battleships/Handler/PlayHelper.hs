module Handler.PlayHelper where

import Import
import Data.Array as A
import Data.Maybe as M

import Diagrams.Prelude

import Logic.GameFramework
import Logic.Rendering

import Data.Serialize
import Data.ByteString as BS


writeSession :: MonadHandler m => PlayerGrid -> m ()
writeSession grid = do
  setSessionBS "playerGrid" $ encode grid

renderFireShotGrid :: MonadHandler m => m BattleDia
renderFireShotGrid = do
  (fleet, grid) <- readSession
  return $ renderPlayerGrid fleet grid

readSession :: MonadHandler m => m (Fleet, PlayerGrid)
readSession = do
  -- read fleet from session cookie:
  fleetSerialized <- lookupSessionBS "fleetPending"
  let fleet = readFleet fleetSerialized
  -- read player grid from session cookie:
  gridSerialized <- lookupSessionBS "playerGrid"
  let grid = readGrid gridSerialized
  return (fleet, grid) where
    readFleet :: Maybe BS.ByteString -> Fleet
    readFleet serialized = case serialized of
      Nothing -> []
      Just s -> either (const []) id $ decode s
    readGrid :: Maybe BS.ByteString -> PlayerGrid
    readGrid serialized = case serialized of
      Nothing -> A.listArray ((0,0),(9,9)) (repeat False)
      Just s -> either (const $ A.listArray ((0,0),(9,9)) (repeat False)) id $ decode s

coordinatesToPos :: Double -> Double -> BattleDia -> Maybe Pos
coordinatesToPos x y grid = M.listToMaybe . sample grid $ p2 (x, y)

handleShot :: Fleet -> PlayerGrid -> Pos -> PlayerGrid
handleShot fleet grid pos = case shipAt fleet pos of
                              Nothing -> grid
                              Just _  -> grid // [(pos, True)]
