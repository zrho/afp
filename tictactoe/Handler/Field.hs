module Handler.Field (getFieldR, posInPicture, picture) where

import Import

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))
import GameLogic.TicTacToe
import Data.Maybe

getFieldR :: TicTacToe -> Handler TypedContent
getFieldR
  = return
  . TypedContent typeSvg
  . toContent
  . renderDia SVG (SVGOptions Absolute Nothing)
  . picture

-------------------------------------------------------------------------
-- * Field picture

posInPicture :: TicTacToe -> (Double, Double) -> Maybe Pos
posInPicture f (px, py)
  = listToMaybe 
  $ sample (picture f)
  $ p2 (px, py)

picture :: TicTacToe -> QDiagram SVG R2 [Pos]
picture = alignTL . pad 1.05 . picture'

picture' :: TicTacToe -> QDiagram SVG R2 [Pos]
picture' f = mconcat $ fmap (\p -> boxT p $ getField f p) positions where
  boxT (x, y) p
    = box p
    # translateX x'
    # translateY y'
    # value [(x, y)] where
      x' = boxSize * (1 - fromIntegral x) -- (1 - ...) for centering the origin
      y' = boxSize * (1 - fromIntegral y)

  box p = square boxSize # lw 5 <> playerSymbol p

  playerSymbol Nothing  = mempty
  playerSymbol (Just X) = stroke ((topLeft ~~ bottomRight) <> (bottomLeft ~~ topRight))
                      # lw lnWidth # lc blue
  playerSymbol (Just O) = circle (half - lnWidth)
                      # lw lnWidth # lc red
  boxSize = 100 -- size of each box
  lnWidth = 5   -- line width
  half = boxSize / 2
  topLeft = origin .+^ ((-half) & half)
  bottomRight = origin .+^ (half & (-half))
  bottomLeft = origin .+^ ((-half) & (-half))
  topRight = origin .+^ (half & half)

