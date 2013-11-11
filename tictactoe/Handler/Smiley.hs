{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Smiley (getSmileyR) where

import Import
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))



getSmileyR :: Handler TypedContent
getSmileyR = 
    return
  . TypedContent typeSvg
  . toContent
  . renderDia SVG (SVGOptions Absolute Nothing)
  $ (smileyPic # scale 100 :: QDiagram SVG R2 Any)

--smileypic :: QDiagram SVG R2 [(Int, Int)]
smileyPic = 
  eye # translate (0.45 & 0.3) `atop`
  eye # translate ((-0.45) & 0.3) `atop`
  mouth # translate (0 & (-0.4)) `atop`
  face # pad 1.1
  where
    mouth = square 1 # fc black # scaleY 0.1
    eye = circle 0.1 # fc black # lw 0
    face = circle 1 # fc white # lw 5
