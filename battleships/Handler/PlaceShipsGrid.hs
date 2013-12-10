{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlaceShipsGrid (getPlaceShipsGridR) where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.GameFramework
import Logic.Rendering

import Data.Serialize
import Data.List as L
import Data.ByteString as BS

import Handler.PlaceShipsHelper

getPlaceShipsGridR :: Handler TypedContent
getPlaceShipsGridR = do
  grid <- renderPlaceShipsGrid
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) grid
