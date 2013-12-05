{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.TrackingGrid (getTrackingGridR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Logic.Rendering

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getTrackingGridR :: Handler TypedContent
getTrackingGridR = do
  let pic = referenceField 10 10
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) pic