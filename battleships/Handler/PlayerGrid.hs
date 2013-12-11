{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlayerGrid (getPlayerGridR) where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.Game
import Logic.Render

getPlayerGridR :: Handler TypedContent
getPlayerGridR = do
  let g = A.listArray ((0,0),(9,9)) (cycle [True, True, False, False])
  let f = [Ship (1,1) 4 Horizontal, Ship (7,8) 2 Vertical, Ship (5, 3) 5 Vertical]
  let pic = renderPlayerGrid f g
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) pic