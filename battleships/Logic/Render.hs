----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Render
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- Diagrams based renderer for the game board.

{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Logic.Render
  ( renderReferenceGrid
  , renderEnemyGrid
  , renderPlayerGrid
  , renderLegend
  , renderGrid
  , LegendIcon (..)
  , BattleDia
  ) where

import           Prelude
import           Logic.Game
import qualified Data.Map as Map
import qualified Data.List as L
import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Diagrams.TwoD.Text
import           Data.Colour.SRGB
import           Data.Foldable (fold, foldMap)
import           Data.Function (on)
import           Data.Ix
import           Data.Maybe

type BattleDia = QDiagram SVG R2 [Pos]

-- | Arrows displayed on the bow and stern of undamaged ships.
data MoveArrow = ArrowRight | ArrowUp | ArrowLeft | ArrowDown deriving Enum

-- | legend icon to render
data LegendIcon 
  = LIShipWithArrow 
  | LIShipMovable
  | LIShipImmovable
  | LIShipHit
  | LIShipSunk
  | LIFogOfWar
  | LIWater
  | LILastShot
  deriving (Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- * Legend Rendering
-------------------------------------------------------------------------------

renderLegend :: LegendIcon -> QDiagram SVG R2 Any
renderLegend icon = case icon of
  LIShipWithArrow -> renderArrow ArrowRight <> movableSquare
  LIShipMovable   -> movableSquare
  LIShipImmovable -> shipSquare
  LIShipHit       -> marker # lc markerHitColor <> shipSquare
  LIShipSunk      -> marker # lc markerSunkColor <> shipSquare
  LIFogOfWar      -> square cellSize # fc fogColor
  LIWater         -> waterSquare
  LILastShot      -> square cellSize # alignTL <> lastShotMarker 1

-------------------------------------------------------------------------------
-- * High-Level Rendering for Grids
-------------------------------------------------------------------------------

renderReferenceGrid :: BattleDia
renderReferenceGrid = renderGrid

renderEnemyGrid :: Fleet -> TrackingList -> Rules -> Int -> Bool -> BattleDia
renderEnemyGrid fleet shots Rules{..} turnNumber uncoverFleet = mconcat
  [ if uncoverFleet then renderFleetHints else mempty
  , if rulesNoviceMode then mconcat (fmap renderImpossiblePositions nonWaterShots) else mempty
  , mconcat (fmap renderShot shots)
  , contentSquare # fc fogColor
  ]
  where
    opac = timedOpacity rulesMove turnNumber
    renderShot (Shot pos val time) = translateToPos pos . value [] . alignTL $
      case val of
        Water -> waterSquare # opac time
        Hit   -> if isShipAtSunk fleet pos
          then waterSquare # opac (fromJust $ sinkTime fleet pos shots)
          else marker # lc markerHitColor <> shipSquare
        Sunk  -> waterSquare # opac time
    renderFleetHints = fold $ fmap renderFleetHint fleet
    renderFleetHint Ship{shipShape = ShipShape{shipPosition=(x,y),..}} =
      let
        (w,h) = case shipOrientation of
          Horizontal -> (realToFrac shipSize, 1)
          Vertical   -> (1, realToFrac shipSize)
      in rect (w * cellSize) (h * cellSize) # alignTL # translateToPos (x,y) # lc red # lw 1 # value []
    renderImpossiblePositions (Shot p _ _) = mconcat (fmap (renderImpossiblePos p) $ marginPositions p)
    renderImpossiblePos hitPos impPos = translateToPos impPos $ value [] $ alignTL $
      if rulesMove && isShipAtSunk fleet hitPos
        then waterSquare # opac (fromJust $ sinkTime fleet hitPos shots)
        else waterSquare
    marginPositions (x,y) = filter (inRange gridRange) [(x+i, y+j) | i <- [-1,1], j <- [-1,1]]
    gridRange             = ((0, 0), (fst boardSize - 1, snd boardSize - 1))
    nonWaterShots         = filter (\s -> shotResult s /= Water) shots

renderPlayerGrid :: Fleet -> TrackingList -> Action -> Rules -> Int -> BattleDia
renderPlayerGrid fleet shots requiredAction Rules{..} turnNumber = mconcat
    [ markLastShots
    , fold $ fmap renderShip $ Map.filter (not . isDamaged) fleet -- show movable ships on top ...
    , fold $ fmap renderShot $ filter ((/=Water) . shotResult) shots
    , fold $ fmap renderShip $ Map.filter isDamaged fleet         -- ... damaged ones below
    , fold $ fmap renderShot $ filter ((==Water) . shotResult) shots
    , contentSquare # fc waterColor
    ]
  where
    opac = timedOpacity rulesMove turnNumber
    markLastShots = case L.groupBy ((==) `on` shotTime) shots of
      shotsLastRound:_ 
        -> flip foldMap (zip [1::Int ..] (reverse shotsLastRound)) $ \(idx, Shot lastShotPos _ _) ->
                 lastShotMarker idx # translateToPos lastShotPos # value []
      _ -> mempty # value []

    renderShip ship@Ship{shipShape = ShipShape{shipPosition=(x,y),..},..} = 
      translateToPos (x,y) $ value [] $ case shipOrientation of
        Horizontal -> hcat [shipCell i | i <- [0..shipSize-1]] # alignTL
        Vertical   -> vcat [shipCell i | i <- [0..shipSize-1]] # alignTL
      where
        shipCell = if isDamaged ship then const shipSquare else movableShipCell
        movableShipCell i =
          case requiredAction of
            ActionMove -> maybe mempty renderArrow (movementArrowAt ship i fleet) <> movableSquare
            _          -> movableSquare
        
    renderShot (Shot pos val time) = translateToPos pos . value [] . alignTL $
      case val of
        Water -> marker # lc markerWaterColor # opac time <> waterSquare
        Hit   -> if isShipAtSunk fleet pos
          then marker # lc markerWaterColor # opac time <> waterSquare
          else marker # lc markerHitColor <> shipSquare
        Sunk  -> marker # lc markerWaterColor # opac time <> waterSquare

timedOpacity :: (Integral i, HasStyle c) => Bool -> i -> i -> c -> c
timedOpacity True turnNumber shotTime
  | (turnNumber - shotTime) < 18 = opacity $ 0.05 * fromIntegral (20 + shotTime - turnNumber)
  | otherwise                    = opacity 0.1
timedOpacity False _ _           = opacity 1

-------------------------------------------------------------------------------
-- * Low-Level Rendering
-------------------------------------------------------------------------------

rowNumbers :: Int -> BattleDia
rowNumbers n = vcat [num i | i <- [1..n]] # value [] where
  num i = (text (show i) # fc gridColor # numberStyle) <> square cellSize

colNumbers :: Int -> BattleDia
colNumbers n = hcat [num i | i <- [0..n-1]] # value [] where
  num i = (text (strNum i) # fc gridColor # numberStyle) <> square cellSize
  strNum :: Int -> String
  strNum i = [toEnum $ fromEnum 'A' + i]

#if MIN_VERSION_diagrams_lib(0,7,0)
marker, waterSquare, shipSquare, movableSquare
  :: (Alignable b, HasOrigin b, TrailLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#else
marker, waterSquare, shipSquare, movableSquare
  :: (Alignable b, HasOrigin b, PathLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#endif
marker         = lw 3 $ drawX (markerRadius * sqrt 2) <> circle markerRadius where
  drawX s      = p2 (-0.5 * s, -0.5 * s) ~~ p2 (0.5 * s, 0.5 * s)
               <> p2 (-0.5 * s, 0.5 * s) ~~ p2 (0.5 * s, -0.5 * s)
waterSquare    = square cellSize # fc waterColor
shipSquare     = rect cellSize cellSize # fc shipColor
movableSquare  = rect cellSize cellSize # fc movableColor

lastShotMarker :: (Renderable (Path R2) b, Renderable Text b) 
               => Int -> Diagram b R2
lastShotMarker idx = 
    (   rect (cellSize - 4) (cellSize - 4) # lc lastShotColor # lw 3
     <> text (show idx) # numberStyle # fc white
    ) # alignTL # translate (r2 (2,-2))

contentSquare :: BattleDia
contentSquare
  = rect (cellSize * realToFrac nx) (cellSize * realToFrac ny)
  # alignTL
  # value []
  # translateToPos (0,0)
  where
    (nx, ny) = boardSize

movementArrowAt :: Ship -> Int -> Fleet -> Maybe MoveArrow
movementArrowAt ship@Ship{..} i fleet =
  case shipOrientation shipShape of
    Horizontal
      | i == 0                      && canMove Forward  -> Just ArrowLeft
      | i == shipSize shipShape - 1 && canMove Backward -> Just ArrowRight
    Vertical
      | i == 0                      && canMove Forward  -> Just ArrowUp
      | i == shipSize shipShape - 1 && canMove Backward -> Just ArrowDown
    _ -> Nothing
    where 
      canMove dir = isMovable dir fleet ship

renderArrow :: MoveArrow -> QDiagram SVG R2 Any
renderArrow arrType = arrowShape # rotateBy circleFraction # arrowStyle  where 
  circleFraction = fromIntegral (fromEnum arrType) / 4
  arrowShape     = fromVertices [ p2 (0, -0.8 * halfCellSize)
                                , p2 (0.8 * halfCellSize,  0)
                                , p2 (0,  0.8 * halfCellSize)]

translateToPos :: (Transformable t, V t ~ R2) => Pos -> t -> t
translateToPos (x,y) = 
  let 
    (nx, ny) = (cellSize + realToFrac x * cellSize, - cellSize - realToFrac y * cellSize)
  in translate (r2 (nx,ny))

-------------------------------------------------------------------------------
-- * Grid Rendering
-------------------------------------------------------------------------------

renderGrid :: BattleDia
renderGrid  = border <> gridLines <> labels where
  border    = rect w h # alignTL # lw 1 # lc gridColor # value []
  gridLines = innerLines <> outerLines
  xnums     = colNumbers nx # translate (r2 (cellSize, 0))
  ynums     = rowNumbers ny
  field     = vcat [cellRow y | y <- [0..nx-1]]
  cellRow y = hcat [ posSquare (x,y) | x <- [0..ny-1]]
  -- invisible square carrying the position in grid-coordinates
  posSquare p = square cellSize # value [p]
  labels    = vcat [xnums, hcat [ynums, field, ynums], xnums] # alignTL
  w = (fromIntegral nx + 2) * cellSize
  h = (fromIntegral ny + 2) * cellSize
  outerOffsets n = [cellSize, (fromIntegral n + 1) * cellSize]
  innerOffsets n = [fromIntegral i * cellSize | i <- [2..n]]
  innerLines = (xticks (h-1) (innerOffsets nx) <> yticks (w-1) (innerOffsets ny)) # innerGridLineStyle # value []
  outerLines = (xticks (h-1) (outerOffsets nx) <> yticks (w-1) (outerOffsets ny)) # outerGridLineStyle # value []
  (nx, ny)   = boardSize

#if MIN_VERSION_diagrams_lib(0,7,0)
xticks, yticks :: (Monoid a, TrailLike a, V a ~ R2) 
               => Double -> [Double] -> a
#else
xticks, yticks :: (Monoid a, PathLike a, V a ~ R2) 
               => Double -> [Double] -> a
#endif
xticks h xs = mconcat [fromVertices [p2 (x, 0), p2 (x, -h) ] | x <- xs]
yticks w ys = mconcat [fromVertices [p2 (0, -y), p2 (w, -y) ] | y <- ys]

-------------------------------------------------------------------------------
-- * Style Constants
-------------------------------------------------------------------------------

cellSize, halfCellSize, markerRadius, innerGridWidth :: Double
cellSize       = 40
halfCellSize   = cellSize / 2
markerRadius   = cellSize / 2 - 3
innerGridWidth = 1

innerGridLineStyle, outerGridLineStyle, arrowStyle :: HasStyle c => c -> c
innerGridLineStyle = lw innerGridWidth . lc gridColor . dashing [3, 3] 0
outerGridLineStyle = lw innerGridWidth . lc gridColor
arrowStyle         = lw 3 . lc gray

numberStyle :: HasStyle c => c -> c
numberStyle = fontSize 30 . font "Monospace"

gridColor, fogColor, waterColor, markerHitColor, markerSunkColor, markerWaterColor, 
  shipColor, lastShotColor, movableColor
  :: Colour Double
gridColor        = sRGB24 0xD2 0xF8 0x70
fogColor         = sRGB 0.7 0.7 0.7
waterColor       = sRGB24 0x36 0xBB 0xCE
markerHitColor   = sRGB24 0xFE 0x3F 0x44
markerSunkColor  = sRGB24 0xA4 0x00 0x04
markerWaterColor = sRGB24 0x00 0x00 0xFF
shipColor        = gray
lastShotColor    = red
movableColor     = sRGB24 0xC4 0xF8 0x3E
