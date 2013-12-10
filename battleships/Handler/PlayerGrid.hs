{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlayerGrid (getPlayerGridR) where

import Import
import Data.Array as A

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.GameFramework
import Logic.Rendering
import Handler.PlayHelper

import Data.Serialize
import Data.ByteString as BS

getPlayerGridR :: Handler TypedContent
getPlayerGridR = do
  (f, g) <- readSession
  let pic = renderPlayerGrid f g
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) pic

