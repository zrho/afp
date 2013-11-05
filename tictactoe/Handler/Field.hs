module Handler.Field (getFieldR, posInPicture, picture) where

import Import

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
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
picture = alignTL . scale 30 . picture'

picture' :: TicTacToe -> QDiagram SVG R2 [Pos]
picture' f = mconcat $ fmap (\p -> boxT p $ getField f p) positions where
  boxT (x, y) p
    = box p
    # translateX x'
    # translateY y'
    # value [(x, y)] where
      x' = fromIntegral x
      y' = 3 - fromIntegral y
  box p = square 1 # lw 0.2 <> player p
  player Nothing  = mempty
  player (Just O) = text "O" # translateY (-0.1)
  player (Just X) = text "X" # translateY (-0.1)
