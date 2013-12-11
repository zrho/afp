{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlaceShipsGrid (getPlaceShipsGridR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Handler.PlaceShipsHelper

getPlaceShipsGridR :: Handler TypedContent
getPlaceShipsGridR = do
  grid <- renderPlaceShipsGrid
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) grid
