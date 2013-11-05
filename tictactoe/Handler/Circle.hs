module Handler.Circle (getCircleR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

getCircleR :: Handler TypedContent
getCircleR
  = return
  $ TypedContent typeSvg
  $ toContent
  $ renderDia SVG (SVGOptions Absolute) -- SVGOptions takes 1 arg in 0.7
  $ (picture :: QDiagram SVG R2 Any)
  where picture = circle 20 # lw 1 # scale 2 # pad 1.1
