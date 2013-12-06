module Logic.Rendering where

import Prelude

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB

import Logic.GameFramework


type BattleDia = QDiagram SVG R2 [Pos]


referenceField :: Int -> Int -> BattleDia
referenceField nx ny = grid nx ny <> cells nx ny

cells :: Int -> Int -> BattleDia
cells nx ny = vcat [xnums, hcat [ynums, field, ynums], xnums] # alignTL where
  xnums     = colNumbers nx # translate (r2 (cellSize, 0))
  ynums     = rowNumbers ny
  field     = vcat [cellRow y | y <- [0..nx-1]]
  cellRow y = hcat [cell (x,y) | x <- [0..ny-1]]
  cell pos  = square 40 # value [pos] # cellStyle pos
  cellStyle (x,y)
    | (x + y) `mod` 2 == 0 = fc (sRGB24 0x00 0x99 0xFF)
    | otherwise            = fc (sRGB24 0x99 0xCC 0xFF)

rowNumbers :: Int -> BattleDia
rowNumbers n = vcat [num i | i <- [1..n]] # value [] where
  num i = (text (show i) # lc black # numberStyle) <> square cellSize

colNumbers :: Int -> BattleDia
colNumbers n = hcat [num i | i <- [0..n-1]] # value [] where
  num i = (text (strNum i) # lc black # numberStyle) <> square cellSize
  strNum :: Int -> String
  strNum i = [toEnum $ fromEnum 'A' + i]

-- * this is just for the grid lines
grid :: Int -> Int -> BattleDia
grid nx ny = (innerLines <> outerLines) # alignTL where
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