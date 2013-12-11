{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.PlayerGrid (getPlayerGridR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.Rendering
import Handler.PlayHelper

getPlayerGridR :: Handler TypedContent
getPlayerGridR = do
  (f, g) <- readSession
  let pic = renderPlayerGrid f g
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute Nothing) pic

