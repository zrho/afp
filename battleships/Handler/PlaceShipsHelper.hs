module Handler.PlaceShipsHelper where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.GameFramework
import Logic.Rendering
import Logic.Session

import Data.Serialize
import Data.List as L
import Data.Maybe as M
import Data.ByteString as BS

renderPlaceShipsGrid :: MonadHandler m => m BattleDia
renderPlaceShipsGrid = do
  rules <- readFromSessionDefault rulesKey standardRules
  fleet <- readFromSessionDefault fleetPendingKey []
  let (width,height) = mapSize rules
  let g = A.listArray ((0,0),(width - 1, height - 1)) (repeat False)
  return $ renderPlayerGrid fleet g

coordinatesToPos :: Double -> Double -> BattleDia -> Maybe Pos
coordinatesToPos x y grid = M.listToMaybe . sample grid $ p2 (x, y)

