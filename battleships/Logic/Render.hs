{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Logic.Render
  ( renderEnemyGrid
  , renderPlayerGrid
  , renderPlaceGrid
  , BattleDia
  ) where

import Prelude
import Logic.Game
import Control.Monad
import Data.Array
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB

type BattleDia = QDiagram SVG R2 [Pos]

-- | Arrows displayed on the bow and stern of undamaged ships.
data MoveArrow = ArrowRight | ArrowUp | ArrowLeft | ArrowDown deriving (Show, Eq, Ord, Bounded, Enum)

-------------------------------------------------------------------------------
-- * High-Level Rendering
-------------------------------------------------------------------------------

renderEnemyGrid :: TrackingGrid -> BattleDia
renderEnemyGrid (grid, mLastPos) = renderGrid nx ny <> cells nx ny renderCell where
  (nx,ny)        = gridSize grid
  renderCell pos = value [] $ markedSquare (isLastPos pos mLastPos) $ case grid ! pos of
    Nothing    -> fogSquare
    Just Water -> waterSquare 
    Just Hit   -> marker # lc markerHitColor # lw 3 <> shipSquare <> waterSquare
    Just Sunk  -> marker # lc markerSunkColor # lw 3 <> shipSquare <> waterSquare 

renderPlayerGrid :: Fleet -> TrackingGrid -> BattleDia
renderPlayerGrid fleet tgrid@(grid, mLastPos) = renderGrid nx ny <> cells nx ny renderCell where
  (nx,ny)          =  gridSize grid
  renderCell pos   = value [] $ markedSquare (isLastPos pos mLastPos) $ case (grid ! pos, shipAt fleet pos) of
    (Nothing, Nothing) -> waterSquare 
    (Just _, Nothing)  -> marker # lc markerWaterColor # lw 3 <> waterSquare
    (Nothing, Just s)  -> 
      if (isDamaged tgrid s) 
        then shipSquare 
        else maybe mempty renderArrow (movementArrowAt fleet tgrid pos) <> movableSquare
    (Just _, Just _)   -> square cellSize # fc burningShipColor

renderPlaceGrid :: Fleet -> (Int, Int) -> BattleDia
renderPlaceGrid fleet gSize = renderPlayerGrid fleet $ (newGrid gSize Nothing, Nothing)

-------------------------------------------------------------------------------
-- * Low-Level Rendering
-------------------------------------------------------------------------------

-- | Draws the cells of the field.
cells :: Int                      -- ^ field width
      -> Int                      -- ^ field height
      -> ((Int,Int) -> BattleDia) -- ^ function for rendering the cells contents
      -> BattleDia

cells nx ny drawContents = vcat [xnums, hcat [ynums, field, ynums], xnums] # alignTL where
  xnums     = colNumbers nx # translate (r2 (cellSize, 0))
  ynums     = rowNumbers ny
  field     = vcat [cellRow y | y <- [0..nx-1]]
  cellRow y = hcat [ posSquare (x,y) <> drawContents (x,y) | x <- [0..ny-1]]
  -- invisible square carrying the position in grid-coordinates
  posSquare p = square cellSize # value [p]

rowNumbers :: Int -> BattleDia
rowNumbers n = vcat [num i | i <- [1..n]] # value [] where
  num i = (text (show i) # lc black # numberStyle) <> square cellSize

colNumbers :: Int -> BattleDia
colNumbers n = hcat [num i | i <- [0..n-1]] # value [] where
  num i = (text (strNum i) # lc black # numberStyle) <> square cellSize
  strNum :: Int -> String
  strNum i = [toEnum $ fromEnum 'A' + i]

#if MIN_VERSION_diagrams_lib(0,7,0)
marker, waterSquare, shipSquare, fogSquare, lastShotMarker
  :: (TrailLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#else
marker, waterSquare, shipSquare, fogSquare, lastShotMarker
  :: (PathLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#endif
marker         = drawX (markerRadius * sqrt 2) <> circle markerRadius where
  drawX s      = p2 (-0.5 * s, -0.5 * s) ~~ p2 (0.5 * s, 0.5 * s)
               <> p2 (-0.5 * s, 0.5 * s) ~~ p2 (0.5 * s, -0.5 * s)
waterSquare    = square cellSize # fc waterColor
shipSquare     = roundedRect cellSize cellSize 0 # fc shipColor
movableSquare  = roundedRect cellSize cellSize 0 # fc movableColor
fogSquare      = square cellSize # fc fogColor
lastShotMarker = roundedRect (cellSize - 3) (cellSize - 3) 0 # lc lastShotColor # lw 3

markedSquare :: Bool -> QDiagram SVG R2 Any -> QDiagram SVG R2 Any
markedSquare True  s = lastShotMarker <> s
markedSquare False s = s

isLastPos :: Pos -> (Maybe Pos) -> Bool
isLastPos _    Nothing    = False
isLastPos pos (Just lPos) = pos == lPos

movementArrowAt :: Fleet -> TrackingGrid -> Pos -> Maybe MoveArrow
movementArrowAt fleet grid pos = do
  ship <- shipAt fleet pos
  let 
    Ship{..} = ship
    (x,y)    = shipPosition
  case shipOrientation of
    Horizontal
      | pos == shipPosition     -> Just ArrowLeft
      | pos == (x+shipSize-1,y) -> Just ArrowRight
    Vertical
      | pos == shipPosition     -> Just ArrowUp
      | pos == (x,y+shipSize-1) -> Just ArrowDown
    _ -> Nothing

renderArrow :: MoveArrow -> QDiagram SVG R2 Any
renderArrow arrType = arrowShape # rotateBy circleFraction # arrowStyle  where 
  circleFraction = (fromIntegral $ fromEnum arrType) / 4
  arrowShape     = fromVertices [ p2 (0, -0.8 * halfCellSize)
                                , p2 (0.8 * halfCellSize,  0)
                                , p2 (0,  0.8 * halfCellSize)]

-------------------------------------------------------------------------------
-- * Grid Rendering
-------------------------------------------------------------------------------

renderGrid :: Int -> Int -> BattleDia
renderGrid nx ny = (innerLines <> outerLines) # alignTL where
  w = (fromIntegral nx + 2) * cellSize
  h = (fromIntegral ny + 2) * cellSize
  outerOffsets n = [cellSize, (fromIntegral n + 1) * cellSize]
  innerOffsets n = [(fromIntegral i) * cellSize | i <- [2..n]]

  innerLines = (xticks h (innerOffsets nx) <> yticks w (innerOffsets ny)) # innerGridLineStyle # value []
  outerLines = (xticks h (outerOffsets nx) <> yticks w (outerOffsets ny)) # outerGridLineStyle # value []

#if MIN_VERSION_diagrams_lib(0,7,0)
xticks, yticks :: (Monoid a, TrailLike a, V a ~ R2) 
               => Double -> [Double] -> a
#else
xticks, yticks :: (Monoid a, PathLike a, V a ~ R2) 
               => Double -> [Double] -> a
#endif
xticks h xs = mconcat [fromVertices [p2 (x, 0), p2 (x, h) ] | x <- xs]
yticks w ys = mconcat [fromVertices [p2 (0, y), p2 (w, y) ] | y <- ys]

-------------------------------------------------------------------------------
-- * Style Constants
-------------------------------------------------------------------------------

cellSize, halfCellSize, markerRadius :: Double
cellSize     = 40
halfCellSize = cellSize / 2
markerRadius = cellSize / 2 - 3

innerGridLineStyle, outerGridLineStyle, arrowStyle :: HasStyle c => c -> c
innerGridLineStyle = lw 1 . lc black . dashing [3, 3] 0
outerGridLineStyle = lw 1 . lc black
arrowStyle         = lw 3 . lc gray

numberStyle :: HasStyle c => c -> c
numberStyle = fontSize 30 . font "Monospace"

fogColor, waterColor, markerHitColor, markerSunkColor,
  markerWaterColor, shipColor, burningShipColor, lastShotColor 
  :: Colour Double

fogColor         = sRGB 0.7 0.7 0.7
waterColor       = sRGB24 0x99 0xCC 0xFF
markerHitColor   = sRGB 1.0 0.5 0.0
markerSunkColor  = sRGB 1.0 0.0 0.0
markerWaterColor = sRGB24 0x33 0x99 0xFF
shipColor        = gray
burningShipColor = orange
lastShotColor    = red
movableColor     = sRGB 0.5 0.7 0.5