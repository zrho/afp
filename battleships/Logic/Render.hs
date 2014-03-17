----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Render
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- Diagrams based renderer for the game board.

{-# LANGUAGE CPP, TypeFamilies #-}
module Logic.Render
  ( renderReferenceGrid
  , renderEnemyGrid
  , renderPlayerGrid
  , renderLegend
  , renderTimedLegend
  , renderGrid
  , LegendIcon (..)
  , TimedLegendIcon (..)
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

data TimedLegendIcon
  = TLIWater Int
  | TLIMarker Int
  deriving (Show)

-------------------------------------------------------------------------------
-- * Legend Rendering
-------------------------------------------------------------------------------

renderLegend :: LegendIcon -> Diagram SVG R2
renderLegend icon = case icon of
  LIShipWithArrow -> renderArrow ArrowRight <> movableSquare
  LIShipMovable   -> movableSquare
  LIShipImmovable -> shipSquare
  LIShipHit       -> marker # lc markerHitColor <> shipSquare
  LIShipSunk      -> marker # lc markerSunkColor <> shipSquare
  LIFogOfWar      -> square cellSize # fc fogColor
  LIWater         -> waterSquare
  LILastShot      -> square cellSize # alignTL <> lastShotMarker 1

renderTimedLegend :: TimedLegendIcon -> Diagram SVG R2
renderTimedLegend icon = case icon of
  TLIWater timeDiff  -> waterSquare # opacityAfter timeDiff <> fogSquare
  TLIMarker timeDiff -> marker # lc markerWaterColor # opacityAfter timeDiff <> waterSquare

-------------------------------------------------------------------------------
-- * High-Level Rendering for Grids
-------------------------------------------------------------------------------

renderReferenceGrid :: BattleDia
renderReferenceGrid = renderGrid

renderEnemyGrid :: Fleet -> TrackingList -> Rules -> Bool -> Int -> Bool -> Diagram SVG R2
renderEnemyGrid fleet shots rules noviceMode turnNumber uncoverFleet = mconcat
  [ if uncoverFleet then renderFleetHints else mempty
  , renderSunkFleet fleet
  , renderPositions $ renderMarker fleet shots rules turnNumber True
  , if noviceMode then renderPositions $ renderImpossible fleet shots rules turnNumber else mempty
  , renderPositions $ renderCell fleet shots rules turnNumber
  , contentSquare # fc fogColor
  ]
  where
    renderFleetHints = lc red
                     . lw 2
                     . dashing [10] 0
                     . renderFleetOutline
                     . Map.filter (not . isShipSunk)
                     $ fleet

renderPlayerGrid :: Fleet -> TrackingList -> Action -> Rules -> Int -> Diagram SVG R2
renderPlayerGrid fleet shots requiredAction rules@Rules{..} turnNumber = mconcat
    [ markLastShots
    , renderSunkFleet fleet
    , renderPositions $ renderMarker fleet shots rules turnNumber False
    , fold . fmap renderShip . Map.filter (not . isShipSunk) $ fleet
    , renderPositions $ renderCell fleet shots rules turnNumber
    , contentSquare # fc waterColor
    ]
  where
    markLastShots = case L.groupBy ((==) `on` shotTime) shots of
      shotsLastRound:_ 
        -> flip foldMap (zip [1::Int ..] (reverse shotsLastRound)) $ \(idx, Shot lastShotPos _ _) ->
                 lastShotMarker idx # translateToPos lastShotPos
      _ -> mempty
    renderShip ship@Ship{shipShape = ShipShape{shipPosition=(x,y),..},..} = 
      translateToPos (x,y) $ case shipOrientation of
        Horizontal -> hcat [shipCell i | i <- [0..shipSize-1]] # alignTL
        Vertical   -> vcat [shipCell i | i <- [0..shipSize-1]] # alignTL
      where
        shipCell = if isDamaged ship then const shipSquare else movableShipCell
        movableShipCell i =
          case requiredAction of
            ActionMove -> maybe mempty renderArrow (movementArrowAt ship i fleet) <> movableSquare
            _          -> movableSquare

gridRange :: (Pos, Pos)
gridRange = ((0, 0), (fst boardSize - 1, snd boardSize - 1))

renderPositions :: (Pos -> Diagram SVG R2) -> Diagram SVG R2
renderPositions f = mconcat . map renderPos $ range gridRange where
  renderPos p = translateToPos p $ f p

-- | Gives the result of the last shot at the given position
-- and the corresponding shot time.
lastShotResult :: Fleet -> TrackingList -> Int -> Pos -> Maybe (HitResponse, Int)
lastShotResult fleet shots turnNumber pos =
  case L.find ((pos == ) . shotPos) shots of
    Nothing -> Nothing
    Just (Shot _ Hit _) -> Just $ case sinkTime fleet pos shots of
      Just t | isShipAtSunk fleet pos -> (Sunk, t)
      _ -> (Hit, turnNumber) -- hit information is always up-to-date -> turnNumber instead of t
    Just (Shot _ res t) -> Just (res, t)

renderMarker :: Fleet -> TrackingList -> Rules -> Int -> Bool -> Pos -> Diagram SVG R2
renderMarker fleet shots Rules{..} turnNumber showOnlyHit pos
 = alignMarker
 $ case lastShotResult fleet shots turnNumber pos of
    Nothing            -> mempty
    Just (Water, time) -> if showOnlyHit then mempty else marker # lc markerWaterColor # opac time
    Just (Hit,   time) -> marker # lc markerHitColor # opac time
    Just (Sunk,  _   ) -> mempty
  where
    opac = timedOpacity rulesMove turnNumber
    alignMarker = translate (r2 (halfCellSize, -halfCellSize))

renderImpossible :: Fleet -> TrackingList -> Rules -> Int -> Pos -> Diagram SVG R2
renderImpossible fleet shots Rules{..} turnNumber pos@(x,y)
 = alignTL $ case impossibleInfo of
    Nothing -> mempty
    Just time -> case lastShotResult fleet shots turnNumber pos of
      Just (_, t) | t >= time -> mempty -- hint is older than actual player's actual information
      _                       -> waterSquare # opac time
  where
    opac = timedOpacity rulesMove turnNumber
    impossibleInfo = findMostRecentHit diagonalCells
      `mostRecent` findMostRecentlySunkShip adjacentCells
    diagonalCells = [(x + dx, y + dy) | dx <- [-1,1], dy <- [-1,1]]
    adjacentCells = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1]]
    findMostRecentHit = foldr (mostRecent . getTimeOfHit) Nothing
    getTimeOfHit p = case lastShotResult fleet shots turnNumber p of
      Just (Hit, time) -> Just time
      _                -> Nothing
    findMostRecentlySunkShip = foldr (mostRecent . \p -> sinkTime sunkFleet p shots) Nothing
    sunkFleet = Map.filter isShipSunk fleet

-- | Finds the most recent time, i.e. chooses the maximum.
mostRecent :: Maybe Int -> Maybe Int -> Maybe Int
mostRecent Nothing t2 = t2
mostRecent t1 Nothing = t1
mostRecent (Just t1) (Just t2) = Just $ max t1 t2

renderCell :: Fleet -> TrackingList -> Rules -> Int -> Pos -> Diagram SVG R2
renderCell fleet shots Rules{..} turnNumber pos
 = alignTL
 $ case lastShotResult fleet shots turnNumber pos of
    Nothing            -> mempty
    Just (Water, time) -> waterSquare # opac time
    Just (Hit,   time) -> shipSquare # opac time
    Just (Sunk,  time) -> waterSquare # opac time
  where
    opac = timedOpacity rulesMove turnNumber


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
marker, waterSquare, shipSquare, movableSquare, fogSquare
  :: (Alignable b, HasOrigin b, TrailLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#else
marker, waterSquare, shipSquare, movableSquare, fogSquare
  :: (Alignable b, HasOrigin b, PathLike b, Transformable b, Semigroup b, HasStyle b, V b ~ R2)
  => b
#endif
marker         = lw 3 $ drawX (markerRadius * sqrt 2) <> circle markerRadius where
  drawX s      = p2 (-0.5 * s, -0.5 * s) ~~ p2 (0.5 * s, 0.5 * s)
               <> p2 (-0.5 * s, 0.5 * s) ~~ p2 (0.5 * s, -0.5 * s)
waterSquare    = square cellSize # fc waterColor
fogSquare      = square cellSize # fc fogColor
shipSquare     = rect cellSize cellSize # fc shipColor
movableSquare  = rect cellSize cellSize # fc movableColor

renderSunkFleet :: Fleet -> Diagram SVG R2
renderSunkFleet = lw 5
                . lc shipColor
                . renderFleetOutline
                . Map.filter isShipSunk

renderFleetOutline :: Fleet -> Diagram SVG R2
renderFleetOutline = fold . fmap renderShipOutline

renderShipOutline :: Ship -> Diagram SVG R2
renderShipOutline Ship {shipShape = ShipShape {shipPosition = (x,y), ..} } =
  rect (w * cellSize) (h * cellSize) # alignTL # translateToPos (x,y) where
    (w,h) = case shipOrientation of
        Horizontal -> (realToFrac shipSize, 1)
        Vertical   -> (1, realToFrac shipSize)

timedOpacity :: (Integral i, HasStyle c) => Bool -> i -> i -> c -> c
timedOpacity True turnNumber shotTime = opacityAfter $ turnNumber - shotTime
timedOpacity False _ _                = opacity 1

opacityAfter :: (Integral i, HasStyle c) => i -> c -> c
opacityAfter timeDiff
  | timeDiff < 20 = opacity $ 0.1 + 0.045 * fromIntegral (20 - timeDiff)
  | otherwise     = opacity 0.1

lastShotMarker :: (Renderable (Path R2) b, Renderable Text b) 
               => Int -> Diagram b R2
lastShotMarker idx = 
    (   rect (cellSize - 4) (cellSize - 4) # lc lastShotColor # lw 3
     <> text (show idx) # numberStyle # fc white
    ) # alignTL # translate (r2 (2,-2))

contentSquare :: Diagram SVG R2
contentSquare
  = rect (cellSize * realToFrac nx) (cellSize * realToFrac ny)
  # alignTL
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

renderArrow :: MoveArrow -> Diagram SVG R2
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
