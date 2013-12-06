module Logic.Rendering where

import Prelude
import Data.Array

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB

import Logic.GameFramework


type BattleDia = QDiagram SVG R2 [Pos]


referenceField :: Int -> Int -> BattleDia
referenceField nx ny = renderGrid nx ny <> cells nx ny (const Nothing)

renderTrackingGrid :: TrackingGrid -> BattleDia
renderTrackingGrid grid = renderGrid nx ny <> cells nx ny renderCellState where
  ((x1,y1),(x2,y2)) = bounds grid
  (nx,ny)           = (x2 - x1 + 1, y2 - y1 + 1)
  renderCellState p = fmap selectDia (grid ! p)
  markerRadius      = cellSize / 2 - 3
  selectDia Water   = (waterSquare) # value []
  selectDia Hit     = (marker # lc (sRGB 1.0 0.5 0.0) # lw 3
                       <> shipSquare <> waterSquare)
                      # value []
  selectDia Sunk    = (marker # lc (sRGB 1.0 0.0 0.0) # lw 3
                       <> shipSquare <> waterSquare)
                      # value []
  diaX size         = p2 (-0.5 * size, -0.5 * size) ~~ p2 (0.5 * size, 0.5 * size)
                    <> p2 (-0.5 * size, 0.5 * size) ~~ p2 (0.5 * size, -0.5 * size)
  marker            = diaX (markerRadius * sqrt 2) <> circle markerRadius
  waterSquare       = square cellSize # fc (sRGB24 0x99 0xCC 0xFF)
  shipSquare        = roundedRect cellSize cellSize 0 # fc gray

cells :: Int -> Int -> ((Int,Int) -> Maybe BattleDia) -> BattleDia
cells nx ny drawContents = vcat [xnums, hcat [ynums, field, ynums], xnums] # alignTL where
  xnums     = colNumbers nx # translate (r2 (cellSize, 0))
  ynums     = rowNumbers ny
  field     = vcat [cellRow y | y <- [0..nx-1]]
  cellRow y = hcat [cell (x,y) | x <- [0..ny-1]]
  cell pos  = case drawContents pos of
    Nothing -> cellBg pos
    Just c  -> c <> cellBg pos
  cellBg p  = square 40 # value [p] # fc (sRGB 0.7 0.7 0.7)


rowNumbers :: Int -> BattleDia
rowNumbers n = vcat [num i | i <- [1..n]] # value [] where
  num i = (text (show i) # lc black # numberStyle) <> square cellSize

colNumbers :: Int -> BattleDia
colNumbers n = hcat [num i | i <- [0..n-1]] # value [] where
  num i = (text (strNum i) # lc black # numberStyle) <> square cellSize
  strNum :: Int -> String
  strNum i = [toEnum $ fromEnum 'A' + i]

-- * this is just for the grid lines
renderGrid :: Int -> Int -> BattleDia
renderGrid nx ny = (innerLines <> outerLines) # alignTL where
  w = (fromIntegral nx + 2) * cellSize
  h = (fromIntegral ny + 2) * cellSize
  outerOffsets n = [cellSize, (fromIntegral n + 1) * cellSize]
  innerOffsets n = [(fromIntegral i) * cellSize | i <- [2..n]]

  innerLines = (xticks h (innerOffsets nx) <> yticks w (innerOffsets ny)) # innerGridLineStyle # value []
  outerLines = (xticks h (outerOffsets nx) <> yticks w (outerOffsets ny)) # outerGridLineStyle # value []

xticks, yticks :: (Monoid a, TrailLike a, V a ~ R2) 
               => Double -> [Double] -> a
xticks h xs = mconcat [fromVertices [p2 (x, 0), p2 (x, h) ] | x <- xs]
yticks w ys = mconcat [fromVertices [p2 (0, y), p2 (w, y) ] | y <- ys]

-- * parameters to tweak rendering
cellSize :: Double
cellSize = 40

innerGridLineStyle, outerGridLineStyle :: HasStyle c => c -> c
innerGridLineStyle = lw 1 . lc black . dashing [3, 3] 0
outerGridLineStyle = lw 1 . lc black

numberStyle :: HasStyle c => c -> c
numberStyle = fontSize 30 . font "Monospace"