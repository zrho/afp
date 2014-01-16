{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Logic.Render
  ( renderReferenceGrid
  , renderEnemyGrid
  , renderPlayerGrid
  , BattleDia
  ) where

import Prelude
import Logic.Game
import Data.Array
import qualified Data.Map as Map
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB
import Data.Foldable (fold)

type BattleDia = QDiagram SVG R2 [Pos]

-- | Arrows displayed on the bow and stern of undamaged ships.
data MoveArrow = ArrowRight | ArrowUp | ArrowLeft | ArrowDown deriving (Show, Eq, Ord, Bounded, Enum)

-------------------------------------------------------------------------------
-- * High-Level Rendering
-------------------------------------------------------------------------------

renderReferenceGrid :: (Int,Int) -> BattleDia
renderReferenceGrid (nx, ny) = renderGrid nx ny

renderEnemyGrid :: (Int,Int) -> TrackingList -> BattleDia
renderEnemyGrid (nx,ny) shots = mconcat
  [ renderGrid nx ny
  , mconcat (fmap renderShot shots)
  , contentSquare nx ny # fc fogColor
  ]
  where
    renderShot (pos, val) = translateToPos pos $ value [] $ alignTL $
      case val of
        Water -> waterSquare
        Hit   -> marker # lc markerHitColor # lw 3 <> shipSquare <> waterSquare
        Sunk  -> marker # lc markerSunkColor # lw 3 <> shipSquare <> waterSquare 

renderPlayerGrid :: (Int,Int) -> Fleet -> TrackingList -> Action -> BattleDia
renderPlayerGrid (nx,ny) fleet shots requiredAction = mconcat
    [ renderGrid nx ny
    , markLastShot
    , fold $ fmap renderShip $ Map.filter (not . isDamaged) fleet -- show movable ships on top ...
    , fold (fmap renderShot shots)
    , fold $ fmap renderShip $ Map.filter isDamaged fleet         -- ... damaged ones below
    , contentSquare nx ny # fc waterColor
    ]
  where
  markLastShot = case shots of
    (lastShotPos,_):_ 
      -> lastShotMarker # value [] # alignTL # translateToPos lastShotPos
    _ -> mempty # value []

  renderShip ship@Ship{shipShape = ShipShape{shipPosition=(x,y),..},..} = 
    translateToPos (x,y) $ value [] $ case shipOrientation of
      Horizontal -> hcat [shipCell i | i <- [0..shipSize-1]] # alignTL
      Vertical   -> vcat [shipCell i | i <- [0..shipSize-1]] # alignTL
    where
      shipCell = if isDamaged ship || requiredAction == ActionFire then defaultShipCell else movableShipCell
      defaultShipCell i = if shipDamage ! i 
        then square cellSize # fc burningShipColor 
        else shipSquare
      movableShipCell i = maybe mempty renderArrow (movementArrowAt (shipShape ship) i) <> movableSquare
      
  renderShot (pos, val) = translateToPos pos $ value [] $ alignTL $
    case val of
      Water -> marker # lc markerWaterColor # lw 3 <> waterSquare
      Hit   -> marker # lc markerHitColor # lw 3 <> shipSquare <> waterSquare
      Sunk  -> marker # lc markerSunkColor # lw 3 <> shipSquare <> waterSquare 

-------------------------------------------------------------------------------
-- * Low-Level Rendering
-------------------------------------------------------------------------------

rowNumbers :: Int -> BattleDia
rowNumbers n = vcat [num i | i <- [1..n]] # value [] where
  num i = (text (show i) # lc black # numberStyle) <> square cellSize

colNumbers :: Int -> BattleDia
colNumbers n = hcat [num i | i <- [0..n-1]] # value [] where
  num i = (text (strNum i) # lc black # numberStyle) <> square cellSize
  strNum :: Int -> String
  strNum i = [toEnum $ fromEnum 'A' + i]

#if MIN_VERSION_diagrams_lib(0,7,0)
marker, waterSquare, shipSquare, movableSquare, lastShotMarker
  :: (TrailLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#else
marker, waterSquare, shipSquare, movableSquare, lastShotMarker
  :: (PathLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#endif
marker         = drawX (markerRadius * sqrt 2) <> circle markerRadius where
  drawX s      = p2 (-0.5 * s, -0.5 * s) ~~ p2 (0.5 * s, 0.5 * s)
               <> p2 (-0.5 * s, 0.5 * s) ~~ p2 (0.5 * s, -0.5 * s)
waterSquare    = square cellSize # fc waterColor
shipSquare     = roundedRect cellSize cellSize 0 # fc shipColor
movableSquare  = roundedRect cellSize cellSize 0 # fc movableColor
lastShotMarker = roundedRect (cellSize - 3) (cellSize - 3) 0 # lc lastShotColor # lw 3

contentSquare :: Int -> Int -> BattleDia
contentSquare nx ny = rect (cellSize * realToFrac nx) (cellSize * realToFrac ny) # alignTL # value [] # translateToPos (0,0)

movementArrowAt :: ShipShape -> Int -> Maybe MoveArrow
movementArrowAt shape i =
  case shipOrientation shape of
    Horizontal
      | i == 0                  -> Just ArrowLeft
      | i == shipSize shape - 1 -> Just ArrowRight
    Vertical
      | i == 0                  -> Just ArrowUp
      | i == shipSize shape - 1 -> Just ArrowDown
    _ -> Nothing

renderArrow :: MoveArrow -> QDiagram SVG R2 Any
renderArrow arrType = arrowShape # rotateBy circleFraction # arrowStyle  where 
  circleFraction = (fromIntegral $ fromEnum arrType) / 4
  arrowShape     = fromVertices [ p2 (0, -0.8 * halfCellSize)
                                , p2 (0.8 * halfCellSize,  0)
                                , p2 (0,  0.8 * halfCellSize)]

translateToPos :: Pos -> BattleDia -> BattleDia
translateToPos (x,y) = 
  let 
    (nx, ny) = (cellSize + realToFrac x * cellSize, - cellSize - realToFrac y * cellSize)
  in translate (r2 (nx,ny))

-------------------------------------------------------------------------------
-- * Grid Rendering
-------------------------------------------------------------------------------

renderGrid :: Int -> Int -> BattleDia
renderGrid nx ny = gridLines <> labels where
  gridLines = (innerLines <> outerLines)
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
xticks h xs = mconcat [fromVertices [p2 (x, 0), p2 (x, -h) ] | x <- xs]
yticks w ys = mconcat [fromVertices [p2 (0, -y), p2 (w, -y) ] | y <- ys]

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

fogColor, waterColor, markerHitColor, markerSunkColor, markerWaterColor, 
  shipColor, burningShipColor, lastShotColor, movableColor
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