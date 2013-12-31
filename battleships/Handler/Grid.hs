{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Grid where

import Import
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Logic.Render
import Logic.Game
import Logic.GameExt
import Handler.Util

getPlayerGridR :: GameStateExt -> Handler TypedContent
getPlayerGridR gameE = withGame gameE $ \(GameState {..}) -> return
  $ renderField $ renderPlayerGrid playerFleet playerImpact

getEnemyGridR :: GameStateExt -> Handler TypedContent
getEnemyGridR gameE = withGame gameE $ \(GameState {..}) -> return
  $ renderField $ renderEnemyGrid playerTrack

renderField :: BattleDia -> TypedContent
renderField
  = TypedContent typeSvg
  . toContent
  . renderSvg
#if MIN_VERSION_diagrams_svg(0,8,0)
  . renderDia SVG (SVGOptions Absolute Nothing)
#else
  . renderDia SVG (SVGOptions Absolute)
#endif