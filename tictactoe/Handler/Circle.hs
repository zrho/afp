module Handler.Circle (getCircleR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getCircleR :: Handler TypedContent
getCircleR =
  let picture = circle 20 # lw 1 # scale 2 # pad 1.1
  in return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) (picture :: QDiagram SVG R2 Any)
