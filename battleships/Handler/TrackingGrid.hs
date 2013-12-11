{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.TrackingGrid (getTrackingGridR) where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.Game
import Logic.Render

getTrackingGridR :: Handler TypedContent
getTrackingGridR = do
  let g = A.listArray ((0,0),(9,9)) (replicate 15 (Just Water) ++ [Just Hit, Just Hit, Just Hit, Just Sunk] ++ repeat (Just Water))
  let pic = renderEnemyGrid g
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) pic